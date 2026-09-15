#!/usr/bin/env ruby
# Run with `brew ruby`: checks the objects Homebrew actually installs, rather
# than approving a package name that a later resolver can interpret differently.
require "json"
require "digest"
require "cmd/upgrade"
require "formulary"
require "cask/cask_loader"

module PersonalUpdatePolicy
  class Rejected < StandardError; end

  def self.canonical(value)
    case value
    when Hash
      value.transform_keys(&:to_s).sort.to_h.transform_values { |v| canonical(v) }
    when Array
      value.map { |v| canonical(v) }
    else
      value
    end
  end

  def self.checked?(checksum)
    checksum.to_s.match?(/\A[0-9a-f]{64}\z/)
  end

  # The bottle Homebrew would pour on this host. Bottles built from a pinned
  # VCS source (git tag, svn revision) carry the only download checksum.
  def self.host_bottle_checksum(formula)
    formula.bottle_specification.collector.specification_for(Utils::Bottles.tag)&.checksum
  end

  def self.describe(kind, item)
    if kind == "brew_formula"
      version = item.pkg_version.to_s
      artifact = [version, item.urls_hash, item.bottle_hash]
      # HEAD builds cannot identify an immutable release. A checked source
      # download or a checked host bottle can; the install guard already refuses
      # source builds, so a bottle checksum alone is sufficient identity.
      supported = !item.head? && (checked?(item.stable&.checksum) || checked?(host_bottle_checksum(item)))
    else
      version = item.version.to_s
      checksum = item.sha256.to_s
      artifact = [version, checksum, item.url.to_s]
      supported = version != "latest" && checked?(checksum)
    end
    {"name" => item.full_name, "available" => version,
     "artifact" => supported ? Digest::SHA256.hexdigest(JSON.generate(canonical(artifact))) : nil}
  end

  # Dependencies Homebrew would install or upgrade together with the item.
  # A formula upgrade brings runtime dependencies that are not at their latest
  # version (FormulaInstaller may accept an older one that satisfies the
  # bottle's recorded minimum, so this errs toward listing more). A cask
  # install brings formulae without a linked installation and casks that are
  # not installed (Cask::Installer#missing_cask_and_formula_dependencies).
  def self.pending_dependencies(kind, item)
    pending = []
    if kind == "brew_formula"
      item.deps.each do |dep|
        next if dep.test? || dep.optional? || dep.build?

        formula = dep.to_formula
        pending << ["brew_formula", formula, formula.latest_version_installed?]
      end
    else
      Array(item.depends_on.formula).each do |dep|
        formula = Formulary.factory(dep)
        pending << ["brew_formula", formula, formula.any_version_installed? && formula.optlinked?]
      end
      Array(item.depends_on.cask).each do |dep|
        cask = Cask::CaskLoader.load(dep)
        pending << ["brew_cask", cask, cask.installed?]
      end
    end
    pending
  end

  def self.observe(request)
    result = {"brew_formula" => {}, "brew_cask" => {}}
    visit = lambda do |kind, item|
      name = item.full_name
      return if result.fetch(kind).key?(name)

      entry = result.fetch(kind)[name] = describe(kind, item)
      dependencies = pending_dependencies(kind, item)
      entry["pending_deps"] = dependencies.reject { |_, _, satisfied| satisfied }
                                          .map { |dep_kind, dep, _| "#{dep_kind}/#{dep.full_name}" }.sort
      dependencies.each { |dep_kind, dep, _| visit.call(dep_kind, dep) }
    end
    request.each do |kind, updates|
      updates.each do |entry|
        item = kind == "brew_formula" ? Formulary.factory(entry.fetch("name")) : Cask::CaskLoader.load(entry.fetch("name"))
        visit.call(kind, item)
        # Keep the observed outdated name as an alias (core casks use full taps).
        result.fetch(kind)[entry.fetch("name")] = result.fetch(kind).fetch(item.full_name)
      end
    end
    result
  end

  def self.configure(allowed)
    @allowed = allowed
    @predecessors = {}
    @restoring = nil
  end

  def self.restoration_identity(cask)
    Digest::SHA256.hexdigest(JSON.generate([cask.full_name, cask.version.to_s, cask.sha256.to_s, cask.url.to_s]))
  end

  def self.with_predecessor(cask)
    key = cask.object_id
    previous = @predecessors[key]
    @predecessors[key] = [cask, restoration_identity(cask)]
    yield
  ensure
    previous ? @predecessors[key] = previous : @predecessors.delete(key)
  end

  def self.with_restoration(cask)
    registered = @predecessors[cask.object_id]
    unless registered && registered[0].equal?(cask) && registered[1] == restoration_identity(cask)
      raise Rejected, "Delayed update refused: cask restoration is not the recorded predecessor"
    end
    previous = @restoring
    @restoring = registered
    begin
      yield
    ensure
      @restoring = previous
    end
  end

  def self.check!(kind, item)
    if kind == "brew_cask" && @restoring && @restoring[0].equal?(item) &&
       @restoring[1] == restoration_identity(item)
      return
    end
    actual = describe(kind, item)
    expected = @allowed.fetch(kind, {}).fetch(actual.fetch("name"), nil)
    unless expected && actual["artifact"] &&
           expected["available"] == actual["available"] && expected["artifact"] == actual["artifact"]
      raise Rejected, "Delayed update refused: #{kind}/#{actual['name']} #{actual['available']} is not age-qualified with this artifact"
    end
  end

  # A formula installer can be created for an implicit dependency or dependent.
  # Validate before initialization/fetch, and again before its install operation.
  module FormulaGuard
    def initialize(formula, *args, **kwargs)
      PersonalUpdatePolicy.check!("brew_formula", formula)
      super
    end

    def install(...)
      PersonalUpdatePolicy.check!("brew_formula", formula)
      raise PersonalUpdatePolicy::Rejected, "Delayed update refused: source builds cannot retain an immutable recipe" unless pour_bottle?
      super
    end

    def build(...)
      raise PersonalUpdatePolicy::Rejected, "Delayed update refused: bottle failure cannot fall back to a source build"
    end

    def install_dependency(dep, dep_formula = dep.to_formula)
      PersonalUpdatePolicy.check!("brew_formula", dep_formula)
      super
    end
  end

  module CaskGuard
    def revert_upgrade(...)
      PersonalUpdatePolicy.with_restoration(cask) { super }
    end

    def fetch(...)
      PersonalUpdatePolicy.check!("brew_cask", cask)
      super
    end

    def install_artifacts(...)
      PersonalUpdatePolicy.check!("brew_cask", cask)
      super
    end
  end

  # An upgrade may uninstall the old cask before installing the new one.
  module CaskUpgradeGuard
    def upgrade_cask(old_cask, new_cask, ...)
      PersonalUpdatePolicy.check!("brew_cask", new_cask)
      PersonalUpdatePolicy.with_predecessor(old_cask) { super }
    end
  end
end

mode, request_path, *upgrade_args = ARGV
request = JSON.parse(File.read(request_path))
case mode
when "observe"
  puts JSON.generate(PersonalUpdatePolicy.observe(request))
when "upgrade"
  PersonalUpdatePolicy.configure(request)
  FormulaInstaller.prepend(PersonalUpdatePolicy::FormulaGuard)
  Cask::Installer.prepend(PersonalUpdatePolicy::CaskGuard)
  Cask::Upgrade.singleton_class.prepend(PersonalUpdatePolicy::CaskUpgradeGuard)
  Homebrew::Cmd::UpgradeCmd.new(upgrade_args).run
  exit(Homebrew.failed? ? 1 : 0)
else
  abort "Unknown personal-updates Homebrew operation"
end
