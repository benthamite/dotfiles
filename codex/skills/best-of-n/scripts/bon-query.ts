#!/usr/bin/env npx tsx
/**
 * Best-of-N query: sample each model N times, pick the best, then synthesise.
 *
 * Usage:
 *   yarn query "What are the pros and cons of TypeScript?"
 *   yarn query -n 4 -T 0.8 -p quick "Explain quantum computing"
 *   yarn query -n 2 -m gpt-5.2,gemini-3-flash "Your question"
 */

import { config as dotenvConfig } from 'dotenv';
dotenvConfig({ override: true });
import { program } from 'commander';
import { generateText } from 'ai';
import { readFileSync, writeFileSync, mkdirSync, existsSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';
import { EventEmitter } from 'events';
import {
  loadConfig,
  createModel,
  getEligibleModels,
  getPresetModels,
  listPresets,
  listModels,
  type Config,
} from './models.js';
import { generateHtmlFromMarkdown } from './html.js';
import {
  compareModelSamples,
  brainstormModelSamples,
  type SampleResult,
} from './per-model-synthesis.js';
import { synthesiseBestResponses } from './cross-model-synthesis.js';
import { notifyBonComplete, notifyBonError } from './notify.js';

const __dirname = dirname(fileURLToPath(import.meta.url));
const SKILL_DIR = join(__dirname, '..');

// ── Helpers ──────────────────────────────────────────────────────────────────

function generateSlug(prompt: string): string {
  return prompt
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-|-$/g, '')
    .slice(0, 50);
}

function generateOutputDir(prompt: string): string {
  const now = new Date();
  const timestamp = now.toISOString().slice(0, 16).replace(/[T:]/g, '-');
  const slug = generateSlug(prompt);
  return join(SKILL_DIR, 'data', 'model-outputs', `${timestamp}-bon-${slug}`);
}

function normaliseHeadings(text: string): string {
  return text.replace(/^# /gm, '## ');
}

type OutputFormat = 'markdown' | 'html' | 'both';

function getHtmlPath(mdPath: string): string {
  return mdPath.replace(/\.md$/, '.html');
}

function syncHtmlFile(mdPath: string, outputFormat: OutputFormat): void {
  if (outputFormat === 'markdown') return;
  try {
    const mdContent = readFileSync(mdPath, 'utf-8');
    const html = generateHtmlFromMarkdown(mdContent, mdPath);
    writeFileSync(getHtmlPath(mdPath), html);
  } catch {
    // Silently skip if markdown file doesn't exist yet
  }
}

// ── Progress tracking ────────────────────────────────────────────────────────

type SampleStatus = 'pending' | 'querying' | 'success' | 'error' | 'timeout';

interface ModelSampleProgress {
  modelName: string;
  displayName: string;
  total: number;
  completed: number;
  failed: number;
  comparing: boolean;
  comparisonDone: boolean;
}

class BonProgressTracker extends EventEmitter {
  private models: Map<string, ModelSampleProgress> = new Map();
  private startTime: number;
  private renderInterval?: ReturnType<typeof setInterval>;
  private lineCount = 0;
  private maxNameLen: number;

  constructor(modelNames: string[], numSamples: number, config: Config) {
    super();
    this.startTime = Date.now();

    let longest = 0;
    for (const name of modelNames) {
      const mc = config.models[name];
      const displayName = mc?.display_name || name;
      if (displayName.length > longest) longest = displayName.length;
      this.models.set(name, {
        modelName: name,
        displayName,
        total: numSamples,
        completed: 0,
        failed: 0,
        comparing: false,
        comparisonDone: false,
      });
    }
    this.maxNameLen = longest;
  }

  start(): void {
    this.renderInterval = setInterval(() => this.render(), 500);
    this.render();
  }

  stop(): void {
    if (this.renderInterval) {
      clearInterval(this.renderInterval);
      this.renderInterval = undefined;
    }
    // Final render to show completed state
    this.render();
  }

  sampleComplete(modelName: string, success: boolean): void {
    const m = this.models.get(modelName);
    if (m) {
      if (success) m.completed++;
      else m.failed++;
    }
  }

  setComparing(modelName: string): void {
    const m = this.models.get(modelName);
    if (m) m.comparing = true;
  }

  setComparisonDone(modelName: string): void {
    const m = this.models.get(modelName);
    if (m) {
      m.comparing = false;
      m.comparisonDone = true;
    }
  }

  getElapsedTime(): string {
    const elapsed = Math.floor((Date.now() - this.startTime) / 1000);
    const mins = Math.floor(elapsed / 60);
    const secs = elapsed % 60;
    return mins > 0 ? `${mins}m ${secs}s` : `${secs}s`;
  }

  private render(): void {
    const R = '\x1b[0m';
    const dim = '\x1b[2m';
    const green = '\x1b[32m';
    const yellow = '\x1b[33m';
    const red = '\x1b[31m';
    const cyan = '\x1b[36m';

    const lines: string[] = [];
    let totalDone = 0;
    let totalAll = 0;
    let allFinished = true;

    for (const m of this.models.values()) {
      const done = m.completed + m.failed;
      totalDone += done;
      totalAll += m.total;

      // Progress bar: █ for success, ✗ for failed, ░ for pending
      const bar =
        green + '█'.repeat(m.completed) + R +
        (m.failed > 0 ? red + '█'.repeat(m.failed) + R : '') +
        dim + '░'.repeat(m.total - done) + R;

      // Status label
      let icon: string;
      let status: string;
      if (m.comparisonDone) {
        icon = `${green}✓${R}`;
        status = `${green}done${R}`;
      } else if (m.comparing) {
        icon = `${cyan}⚡${R}`;
        status = `${cyan}comparing…${R}`;
        allFinished = false;
      } else if (done === m.total) {
        icon = `${yellow}◐${R}`;
        status = `${dim}waiting…${R}`;
        allFinished = false;
      } else {
        icon = done > 0 ? `${yellow}◐${R}` : `${dim}◐${R}`;
        status = `${dim}${done}/${m.total}${R}`;
        allFinished = false;
      }

      if (m.failed > 0) {
        status += `  ${red}${m.failed} failed${R}`;
      }

      const name = m.displayName.padEnd(this.maxNameLen);
      lines.push(`  ${icon} ${name}  ${bar}  ${status}`);
    }

    // Summary footer
    const pct = totalAll > 0 ? Math.round((totalDone / totalAll) * 100) : 0;
    lines.push(`${dim}  ⏱ ${this.getElapsedTime()}  ·  ${totalDone}/${totalAll} samples (${pct}%)${R}`);

    // Move cursor up to overwrite previous render
    if (this.lineCount > 0) {
      process.stdout.write(`\x1b[${this.lineCount}A`);
    }

    for (const line of lines) {
      process.stdout.write(`\x1b[2K${line}\n`);
    }

    this.lineCount = lines.length;
  }

  allComplete(): boolean {
    return [...this.models.values()].every(
      (m) => m.comparisonDone || m.completed + m.failed === m.total,
    );
  }
}

// ── Query a single model once ────────────────────────────────────────────────

interface SingleQueryResult {
  status: 'success' | 'error' | 'timeout';
  response?: string;
  error?: string;
  latencyMs: number;
  tokensUsed?: number;
}

async function querySingleModel(
  modelName: string,
  prompt: string,
  config: Config,
  timeoutMs: number,
  temperature: number,
): Promise<SingleQueryResult> {
  const startTime = Date.now();

  const model = createModel(modelName, config);
  if (!model) {
    return {
      status: 'error',
      error: `Could not create model instance for ${modelName}`,
      latencyMs: Date.now() - startTime,
    };
  }

  const modelConfig = config.models[modelName];
  const maxTokens = modelConfig?.max_tokens || config.defaults.max_tokens;

  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), timeoutMs);

  try {

    // Build generateText options
    const options: Parameters<typeof generateText>[0] = {
      model,
      prompt,
      maxOutputTokens: maxTokens,
      abortSignal: controller.signal,
    };

    // Add temperature — reasoning models may silently ignore it, which is fine.
    // We set it for all models so non-reasoning models get the diversity benefit.
    options.temperature = temperature;

    // Add provider-specific options for reasoning models
    if (modelConfig?.reasoning && modelConfig.provider === 'anthropic') {
      options.providerOptions = {
        anthropic: {
          thinking: { type: 'enabled', budgetTokens: 10000 },
        },
      };
    }

    const result = await generateText(options);

    clearTimeout(timeout);

    return {
      status: 'success',
      response: result.text,
      latencyMs: Date.now() - startTime,
      tokensUsed: result.usage?.totalTokens,
    };
  } catch (error) {
    clearTimeout(timeout);
    const latencyMs = Date.now() - startTime;

    if (error instanceof Error && error.name === 'AbortError') {
      return {
        status: 'timeout',
        error: `Timeout after ${timeoutMs}ms`,
        latencyMs,
      };
    }

    return {
      status: 'error',
      error: error instanceof Error ? error.message : String(error),
      latencyMs,
    };
  }
}

// ── Live file management ─────────────────────────────────────────────────────

function createLiveFile(
  filePath: string,
  prompt: string,
  modelNames: string[],
  numSamples: number,
  temperature: number,
  config: Config,
): void {
  const now = new Date().toLocaleString('en-GB', { timeZone: 'Europe/Paris' });

  let content = `# Best-of-N query\n\n`;
  content += `**Prompt:** ${prompt}\n\n`;
  content += `**N:** ${numSamples} samples per model | **Temperature:** ${temperature}\n`;
  content += `**Time:** ${now}\n\n`;
  content += `---\n\n`;

  for (const name of modelNames) {
    const mc = config.models[name];
    const displayName = mc?.display_name || name;
    content += `# ${displayName}\n\n_Sampling ${numSamples} responses..._\n\n---\n\n`;
  }

  writeFileSync(filePath, content);
}

function updateModelSection(
  filePath: string,
  displayName: string,
  sectionContent: string,
): void {
  let content = readFileSync(filePath, 'utf-8');

  // Match the model's H1 section
  const escapedName = displayName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  const pattern = new RegExp(
    `# ${escapedName}\\n\\n[\\s\\S]*?(?=\\n# [^#]|$)`,
  );

  const replacement = `# ${displayName}\n\n${sectionContent}\n\n`;
  content = content.replace(pattern, replacement);
  writeFileSync(filePath, content);
}

function insertSynthesisInLiveFile(
  filePath: string,
  synthesis: string,
): void {
  let content = readFileSync(filePath, 'utf-8');

  const synthesisSection = `# Synthesis\n\n${synthesis}\n\n---\n\n`;

  // Check if synthesis already exists
  const existingMatch = content.match(/# Synthesis\n\n[\s\S]*?\n\n---\n\n/);
  if (existingMatch && existingMatch.index !== undefined) {
    content =
      content.slice(0, existingMatch.index) +
      synthesisSection +
      content.slice(existingMatch.index + existingMatch[0].length);
  } else {
    // Insert after metadata block (after first ---)
    const firstHr = content.indexOf('---\n\n');
    if (firstHr !== -1) {
      const insertPos = firstHr + 5;
      content =
        content.slice(0, insertPos) + '\n' + synthesisSection + content.slice(insertPos);
    }
  }

  writeFileSync(filePath, content);
}

// ── Save results to disk ─────────────────────────────────────────────────────

interface BonModelResult {
  modelName: string;
  displayName: string;
  samples: SampleResult[];
  bestIndex: number;
  comparisonMarkdown: string;
  brainstormSummary?: string;
}

function saveResults(
  outputDir: string,
  prompt: string,
  numSamples: number,
  temperature: number,
  modelResults: BonModelResult[],
  synthesis?: string,
): void {
  mkdirSync(outputDir, { recursive: true });

  // Save raw JSON
  const responsesJson = {
    prompt,
    numSamples,
    temperature,
    timestamp: new Date().toISOString(),
    models: modelResults.map((mr) => ({
      model: mr.modelName,
      displayName: mr.displayName,
      bestIndex: mr.bestIndex,
      samples: mr.samples.map((s) => ({
        index: s.index,
        latencyMs: s.latencyMs,
        tokensUsed: s.tokensUsed,
        responseLength: s.response.length,
      })),
    })),
  };
  writeFileSync(
    join(outputDir, 'responses.json'),
    JSON.stringify(responsesJson, null, 2),
  );

  // Save per-model files
  for (const mr of modelResults) {
    const modelDir = join(outputDir, 'per-model', mr.modelName);
    mkdirSync(modelDir, { recursive: true });

    // Individual samples
    for (const s of mr.samples) {
      writeFileSync(join(modelDir, `sample-${s.index}.md`), s.response);
    }

    // Best response or merged ideas (brainstorm mode)
    if (mr.brainstormSummary) {
      writeFileSync(join(modelDir, 'merged-ideas.md'), mr.brainstormSummary);
    } else if (mr.samples[mr.bestIndex]) {
      writeFileSync(
        join(modelDir, 'best-response.md'),
        mr.samples[mr.bestIndex].response,
      );
    }

    // Comparison
    writeFileSync(join(modelDir, 'comparison.md'), mr.comparisonMarkdown);
  }

  console.log(`\nResults saved to: ${outputDir}`);
}

// ── Main orchestration ───────────────────────────────────────────────────────

async function runQuery(
  prompt: string,
  options: {
    numSamples?: string;
    temperature?: string;
    models?: string;
    preset?: string;
    timeout?: string;
    outputFormat?: string;
    synthesise?: boolean;
    brainstorm?: boolean;
    output?: string;
  },
): Promise<void> {
  const config = loadConfig();

  // Resolve preset config first, then cascade: CLI flag > preset value > hardcoded default
  const presetName = (!options.models) ? (options.preset || 'quick') : undefined;
  const presetConfig = presetName ? config.presets[presetName] : undefined;

  const numSamples = parseInt(
    options.numSamples ?? String(presetConfig?.num_samples ?? 4), 10
  );
  const temperature = parseFloat(
    options.temperature ?? String(presetConfig?.temperature ?? 0.8)
  );
  const timeoutSeconds = parseInt(
    options.timeout ?? String(presetConfig?.timeout_seconds ?? 180), 10
  );
  const timeoutMs = timeoutSeconds * 1000;
  const outputFormat = (options.outputFormat || 'markdown') as OutputFormat;
  const shouldSynthesise = options.synthesise !== false;
  const brainstorm = options.brainstorm ?? presetConfig?.brainstorm ?? false;

  // Temperature range for ultra creative mode
  const temperatureRange = presetConfig?.temperature_range ?? null;

  // Determine models
  let modelNames: string[];

  if (options.models) {
    modelNames = options.models.split(',').map((m) => m.trim());
    // Validate against eligible models
    const eligible = new Set(getEligibleModels(config));
    const invalid = modelNames.filter((m) => !eligible.has(m));
    if (invalid.length > 0) {
      console.error(`Invalid or ineligible models: ${invalid.join(', ')}`);
      console.error('Use "yarn query models" to see eligible models.');
      process.exit(1);
    }
  } else {
    modelNames = getPresetModels(presetName!, config);
    if (modelNames.length === 0) {
      console.error(
        `No eligible models in preset "${presetName}". Deep research and browser models are excluded.`,
      );
      process.exit(1);
    }
  }

  const totalCalls = modelNames.length * numSamples;
  console.log(
    `\n\x1b[36m🎲 Best-of-N${brainstorm ? ' (brainstorm)' : ''}: ${modelNames.length} models × ${numSamples} samples = ${totalCalls} API calls\x1b[0m`,
  );
  const tempDisplay = temperatureRange
    ? `${temperatureRange[0]}→${temperatureRange[1]} (varies per sample)`
    : String(temperature);
  console.log(`Temperature: ${tempDisplay} | Timeout: ${timeoutSeconds}s per call`);

  // Warn about slow models
  const slowModels = modelNames.filter((m) => config.models[m]?.slow);
  if (slowModels.length > 0) {
    console.log(
      `\x1b[33m⚠ Slow models included: ${slowModels.join(', ')} — this will take a while.\x1b[0m`,
    );
  }

  // Generate output dir and live file path early so everything goes in one place
  const outputDir = options.output || generateOutputDir(prompt);
  mkdirSync(outputDir, { recursive: true });
  const liveFilePath = join(outputDir, 'results.md');
  createLiveFile(liveFilePath, prompt, modelNames, numSamples, temperature, config);
  console.log(`Live file: ${liveFilePath}`);
  console.log('');

  // Start progress
  const tracker = new BonProgressTracker(modelNames, numSamples, config);
  tracker.start();

  // Results accumulator
  const allModelResults: BonModelResult[] = [];

  // Process each model: run N samples, then compare
  const modelPromises = modelNames.map(async (modelName) => {
    const mc = config.models[modelName];
    const displayName = mc?.display_name || modelName;
    const modelTimeout = (mc?.timeout_seconds || timeoutSeconds) * 1000;

    // Per-sample temperature variation (for ultra creative mode)
    const getTemperatureForSample = (sampleIndex: number): number => {
      if (temperatureRange) {
        const [min, max] = temperatureRange;
        if (numSamples <= 1) return (min + max) / 2;
        return min + (max - min) * (sampleIndex / (numSamples - 1));
      }
      return temperature;
    };

    // Run N samples with small stagger between same-provider calls
    const samplePromises: Promise<SingleQueryResult>[] = [];
    for (let i = 0; i < numSamples; i++) {
      const stagger = i * 100; // 100ms stagger
      samplePromises.push(
        new Promise<SingleQueryResult>((resolve) => {
          setTimeout(async () => {
            const result = await querySingleModel(
              modelName,
              prompt,
              config,
              modelTimeout,
              getTemperatureForSample(i),
            );
            tracker.sampleComplete(modelName, result.status === 'success');
            resolve(result);
          }, stagger);
        }),
      );
    }

    const results = await Promise.all(samplePromises);

    // Collect successful samples
    const samples: SampleResult[] = [];
    for (let i = 0; i < results.length; i++) {
      const r = results[i];
      if (r.status === 'success' && r.response) {
        samples.push({
          index: i,
          response: r.response,
          latencyMs: r.latencyMs,
          tokensUsed: r.tokensUsed,
        });
      }
    }

    if (samples.length === 0) {
      const errors = results
        .filter((r) => r.error)
        .map((r) => r.error)
        .join('; ');
      console.error(`\n  All ${numSamples} samples failed for ${modelName}: ${errors}`);
      tracker.setComparisonDone(modelName);

      // Update live file with error
      updateModelSection(
        liveFilePath,
        displayName,
        `**Error:** All ${numSamples} samples failed.\n\n${errors}`,
      );
      syncHtmlFile(liveFilePath, outputFormat);
      return;
    }

    // Compare samples or extract ideas (brainstorm mode)
    tracker.setComparing(modelName);

    let modelResult: BonModelResult;

    if (brainstorm) {
      const extraction = await brainstormModelSamples(modelName, prompt, samples);
      tracker.setComparisonDone(modelName);

      modelResult = {
        modelName,
        displayName,
        samples,
        bestIndex: 0,
        comparisonMarkdown: extraction.comparisonMarkdown,
        brainstormSummary: extraction.mergedIdeas,
      };
    } else {
      const comparison = await compareModelSamples(modelName, prompt, samples);
      tracker.setComparisonDone(modelName);

      modelResult = {
        modelName,
        displayName,
        samples,
        bestIndex: comparison.bestIndex,
        comparisonMarkdown: comparison.comparisonMarkdown,
      };
    }

    allModelResults.push(modelResult);

    // Update live file
    {
      let section = '';

      if (brainstorm && modelResult.brainstormSummary) {
        section += `## Merged ideas (from ${samples.length} samples)\n\n`;
        section += normaliseHeadings(modelResult.brainstormSummary);
        section += '\n\n';
        section += modelResult.comparisonMarkdown;
        section += '\n';
      } else {
        const bestSample = samples[modelResult.bestIndex];
        section += `## Best response (sample ${bestSample.index + 1} of ${numSamples})\n\n`;
        section += normaliseHeadings(bestSample.response);
        section += `\n\n_Latency: ${(bestSample.latencyMs / 1000).toFixed(1)}s`;
        if (bestSample.tokensUsed) section += ` | Tokens: ${bestSample.tokensUsed}`;
        section += `_\n\n`;
        section += modelResult.comparisonMarkdown;
        section += '\n';
      }

      // All samples in collapsible
      section += `<details><summary>All ${samples.length} samples</summary>\n\n`;
      for (const s of samples) {
        const star = !brainstorm && s.index === modelResult.bestIndex ? ' ★' : '';
        section += `### Sample ${s.index + 1}${star} (${(s.latencyMs / 1000).toFixed(1)}s)\n\n`;
        section += normaliseHeadings(s.response);
        section += '\n\n';
      }
      section += '</details>\n\n---\n';

      updateModelSection(liveFilePath, displayName, section);
      syncHtmlFile(liveFilePath, outputFormat);
    }
  });

  await Promise.all(modelPromises);
  tracker.stop();
  console.log('\n');

  // Sort results to match original model order
  allModelResults.sort(
    (a, b) => modelNames.indexOf(a.modelName) - modelNames.indexOf(b.modelName),
  );

  // Cross-model synthesis
  let synthesis: string | undefined;

  if (shouldSynthesise && allModelResults.length > 0) {
    const bestResponses = allModelResults.map((mr) => ({
      modelName: mr.modelName,
      displayName: mr.displayName,
      response: brainstorm
        ? mr.brainstormSummary || mr.samples[mr.bestIndex]?.response || ''
        : mr.samples[mr.bestIndex]?.response || '',
      sampleCount: mr.samples.length,
      bestIndex: mr.bestIndex,
    }));

    try {
      synthesis = await synthesiseBestResponses(prompt, bestResponses, brainstorm);

      insertSynthesisInLiveFile(liveFilePath, synthesis);
      syncHtmlFile(liveFilePath, outputFormat);
    } catch (err) {
      console.error(
        'Cross-model synthesis failed:',
        err instanceof Error ? err.message : String(err),
      );
      notifyBonError('Cross-model synthesis failed');
    }
  }

  // Save results
  saveResults(outputDir, prompt, numSamples, temperature, allModelResults, synthesis);


  // Print summary
  console.log(`\n\x1b[1m📊 Results Summary\x1b[0m\n`);
  for (const mr of allModelResults) {
    if (mr.brainstormSummary) {
      console.log(`  ✓ ${mr.displayName}: merged ${mr.samples.length} samples`);
    } else {
      const best = mr.samples[mr.bestIndex];
      console.log(
        `  ✓ ${mr.displayName}: best = sample ${best.index + 1} of ${mr.samples.length} (${(best.latencyMs / 1000).toFixed(1)}s)`,
      );
    }
  }

  const failedModels = modelNames.filter(
    (m) => !allModelResults.find((mr) => mr.modelName === m),
  );
  if (failedModels.length > 0) {
    console.log(`  ✗ Failed: ${failedModels.join(', ')}`);
  }
  console.log('');

  // Notify
  notifyBonComplete(allModelResults.length, numSamples, liveFilePath);
}

// ── CLI ──────────────────────────────────────────────────────────────────────

program
  .name('best-of-n')
  .description(
    'Best-of-N sampling: query each model N times, pick the best response, then synthesise',
  )
  .version('0.1.0');

program
  .command('query', { isDefault: true })
  .description('Run best-of-N query')
  .argument('<prompt>', 'The prompt to send to models')
  .option('-n, --num-samples <n>', 'Samples per model', '4')
  .option('-T, --temperature <temp>', 'Temperature for all runs', '0.8')
  .option('-m, --models <list>', 'Comma-separated model IDs')
  .option('-p, --preset <name>', 'Preset (quick, comprehensive — no deep research)')
  .option('-t, --timeout <seconds>', 'Timeout per call in seconds', '180')
  .option('--output-format <format>', 'Output format: markdown, html, both', 'markdown')
  .option('-s, --synthesise', 'Run cross-model synthesis (default: true)')
  .option('--no-synthesise', 'Skip cross-model synthesis')
  .option('-B, --brainstorm', 'Brainstorm mode: merge all unique ideas instead of picking one best')
  .option('-o, --output <dir>', 'Output directory')
  .action(runQuery);

program
  .command('presets')
  .description('List available presets (eligible models only)')
  .action(() => {
    const config = loadConfig();
    listPresets(config);
  });

program
  .command('models')
  .description('List models eligible for best-of-N')
  .action(() => {
    const config = loadConfig();
    listModels(config);
  });

program.parse();
