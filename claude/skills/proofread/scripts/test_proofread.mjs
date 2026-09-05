import assert from 'node:assert/strict';
import { test } from 'node:test';
import { mkdtempSync, readFileSync, writeFileSync, rmSync, existsSync, mkdirSync, symlinkSync, statSync, readdirSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawnSync } from 'node:child_process';
import { applyEdits, splitLines, requestWithDeadline, readSnapshot, publish } from './proofread.ts';

const scripts = dirname(fileURLToPath(import.meta.url));
function fixture(fn) {
  const root = mkdtempSync(join(tmpdir(), 'proofread-fixture-'));
  try { return fn(root); } finally { rmSync(root, { recursive: true }); }
}
function cli(script, args, env = {}) {
  return spawnSync(process.execPath, [join(scripts, script), ...args], {
    encoding: 'utf8', timeout: 10000, env: { PATH: process.env.PATH, ...env },
  });
}
function marker(line, from = 'old', to = '$&', mdx = false, extra = {}) {
  const payload = encodeURIComponent(JSON.stringify({ version: 1, id: 'S1', text: 'Fix', from, to, line, ...extra }));
  const comment = mdx ? `{/* proofread:${payload} */}` : `<!-- proofread:${payload} -->`;
  const suffix = line.match(/[\t ]*$/)[0];
  return line.slice(0, line.length - suffix.length) + ' ' + comment + suffix;
}

test('literal replacement and CRLF/hard breaks survive the public apply CLI', () => fixture(root => {
  const input = join(root, 'doc.proofread.md');
  writeFileSync(input, marker('The old text.  ', 'old', "$& $` $'") + '\r\n');
  const result = cli('apply-suggestions.ts', [input, 'all']);
  assert.equal(result.status, 0, result.stderr);
  assert.equal(readFileSync(join(root, 'doc.final.md'), 'utf8'), "The $& $` $' text.  \r\n");
  assert.deepEqual(JSON.parse(result.stdout).applied, ['S1']);
}));

test('an empty literal replacement is applied, not mislabeled removed', () => fixture(root => {
  const input = join(root, 'doc.proofread.md');
  writeFileSync(input, marker('old text', 'old', ''));
  const result = cli('apply-suggestions.ts', [input, 'S1']);
  assert.equal(result.status, 0, result.stderr);
  assert.equal(readFileSync(join(root, 'doc.final.md'), 'utf8'), ' text');
  assert.deepEqual(JSON.parse(result.stdout).applied, ['S1']);
}));

test('arbitrary input names cannot overwrite their source', () => fixture(root => {
  const input = join(root, 'doc.md');
  const text = marker('old text');
  writeFileSync(input, text);
  assert.notEqual(cli('apply-suggestions.ts', [input, 'all']).status, 0);
  assert.equal(readFileSync(input, 'utf8'), text);
}));

test('unknown selection refuses without an artifact', () => fixture(root => {
  const input = join(root, 'doc.proofread.md');
  writeFileSync(input, marker('old text'));
  assert.notEqual(cli('apply-suggestions.ts', [input, 'S99']).status, 0);
  assert.equal(existsSync(join(root, 'doc.final.md')), false);
}));

test('help is available without a runtime or key', () => {
  const result = cli('proofread.ts', ['--help']);
  assert.equal(result.status, 0, result.stderr);
  assert.match(result.stdout, /Usage:/);
});

function fakeProvider(root, control = {}) {
  const runtime = join(root, 'runtime');
  mkdirSync(join(runtime, 'node_modules', '@ai-sdk', 'google'), { recursive: true });
  mkdirSync(join(runtime, 'node_modules', 'ai'), { recursive: true });
  writeFileSync(join(runtime, 'package.json'), '{}');
  writeFileSync(join(runtime, 'node_modules', '@ai-sdk', 'google', 'index.js'),
    'exports.createGoogleGenerativeAI = options => { if (options.apiKey !== "owned-fixture-key") throw Error("wrong key"); return model => model; };');
  writeFileSync(join(runtime, 'node_modules', 'ai', 'index.js'), `
    const fs = require('node:fs');
    exports.generateText = async options => {
      const control = JSON.parse(fs.readFileSync(process.env.FIXTURE_CONTROL, 'utf8'));
      fs.appendFileSync(process.env.FIXTURE_CALLS, JSON.stringify({model:options.model,prompt:options.prompt,retries:options.maxRetries,signal:!!options.abortSignal})+'\\n');
      if(control.error) throw Error(control.error);
      if(control.changeSource) fs.appendFileSync(control.changeSource, 'changed');
      if(control.lateOutput) fs.writeFileSync(control.lateOutput, 'foreign');
      if(control.lateLink){
        const original = fs.linkSync;
        fs.linkSync = (from,to) => {fs.writeFileSync(to,'foreign-at-link');return original(from,to);};
        require('node:module').syncBuiltinESMExports();
      }
      return {text: control.response === undefined ? '[]' : control.response, finishReason: control.finishReason === undefined ? 'stop' : control.finishReason, toolCalls: control.toolCalls === undefined ? [] : control.toolCalls};
    };`);
  const controlPath = join(root, 'control.json');
  const calls = join(root, 'calls.jsonl');
  writeFileSync(controlPath, JSON.stringify(control));
  return { PROOFREAD_RUNTIME_DIR: runtime, GOOGLE_AI_API_KEY: 'owned-fixture-key', FIXTURE_CONTROL: controlPath, FIXTURE_CALLS: calls };
}
function modelEdit(extra = {}) { return { line: 1, type: 'auto-correction', from: 'bad', to: 'good', reason: 'Fix grammar.', ...extra }; }
function runModel(root, response, text = 'A bad sentence.  \r\n', control = {}) {
  const input = join(root, 'doc.md');
  writeFileSync(input, text);
  const env = fakeProvider(root, { response: typeof response === 'string' ? response : JSON.stringify(response), ...control });
  const result = cli('proofread.ts', [input], env);
  return { input, env, result, output: join(root, 'doc.proofread.md') };
}
function fakeAspell(root, mode = 'normal') {
  const bin = join(root, 'bin'); mkdirSync(bin);
  const calls = join(root, 'aspell-calls.jsonl');
  writeFileSync(join(bin, 'aspell'), `#!${process.execPath}\n
    const fs = require('node:fs'); const input = fs.readFileSync(0, 'utf8');
    fs.appendFileSync(process.env.FIXTURE_ASPELL_CALLS, JSON.stringify({args:process.argv.slice(2),input})+'\\n');
    if(process.env.FIXTURE_ASPELL_MODE==='error'){process.stderr.write('private-child-error');process.exit(7);}
    process.stdout.write('@(#) owned fake aspell\\n');
    if(input.includes('Sentnce')) process.stdout.write('& Sentnce 1 1: Sentence\\n');
    else process.stdout.write('*\\n');
    process.stdout.write('\\n');
  `, { mode: 0o700 });
  return { PATH: bin, FIXTURE_ASPELL_CALLS: calls, FIXTURE_ASPELL_MODE: mode };
}

test('validated model edits count actual applications and preserve source/permissions', () => fixture(root => {
  const { input, output, env, result } = runModel(root, [modelEdit()]);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  assert.equal(readFileSync(input, 'utf8'), 'A bad sentence.  \r\n');
  assert.equal(readFileSync(output, 'utf8'), 'A good sentence.  \r\n');
  assert.equal(statSync(output).mode & 0o777, 0o600);
  assert.equal(JSON.parse(result.stdout).autoApplied.count, 1);
  const calls = readFileSync(env.FIXTURE_CALLS, 'utf8').trim().split('\n').map(JSON.parse);
  assert.equal(calls.length, 1);
  assert.equal(calls[0].model, 'gemini-3.6-flash');
  assert.equal(calls[0].retries, 0);
  assert.equal(calls[0].signal, true);
  assert.equal(readdirSync(root).some(name => name.startsWith('.proofread-')), false);
}));

test('model malformed JSON, schema, unmatched and ambiguous edits never become empty success', () => {
  for (const response of ['not JSON', '{}', '[null]', '[42]', '["text"]', '[] extra', JSON.stringify([modelEdit({type:'unknown'})]),
    JSON.stringify([modelEdit({line:0})]), JSON.stringify([modelEdit({line:2})]), JSON.stringify([modelEdit({from:'absent'})]),
    JSON.stringify([modelEdit({from:'A'})]), JSON.stringify([modelEdit({to:'good\nnew'})]), JSON.stringify([modelEdit({reason:null})])]) {
    fixture(root => {
      const { output, result } = runModel(root, response, 'A bad A sentence.');
      assert.notEqual(result.status, 0, response);
      assert.equal(JSON.parse(result.stdout).status, 'error');
      assert.equal(existsSync(output), false);
      assert.doesNotMatch(result.stderr, /Error:|at file:/);
    });
  }
});

test('provider failure is sanitized and leaves no review artifact', () => fixture(root => {
  const { output, result } = runModel(root, [], 'A bad sentence.', {error:'private-provider-secret-body'});
  assert.notEqual(result.status, 0);
  assert.equal(JSON.parse(result.stdout).error, 'provider_error');
  assert.doesNotMatch(result.stdout + result.stderr, /private-provider-secret-body|owned-fixture-key/);
  assert.equal(existsSync(output), false);
}));

test('deadline aborts an unresponsive request with zero hidden retries', async () => {
  let signal;
  await assert.rejects(requestWithDeadline(options => {
    assert.equal(options.maxRetries, 0); signal = options.abortSignal; return new Promise(() => {});
  }, 10), error => error.code === 'provider_timeout');
  assert.equal(signal.aborted, true);
});

test('all protected regions remain unchanged and are absent from model requests', () => fixture(root => {
  const text = '---\r\nsecret_frontmatter: bad\r\n---\r\nA bad sentence.  \r\n```js\r\nsecret_code bad\r\n```\r\n    secret_indent bad\r\nText `secret_inline` bad\r\n[link](https://secret.invalid/path)\r\n$secret_math$ bad\r\n<!--\r\nsecret_comment bad\r\n-->\r\n<Component>\r\nsecret_jsx bad\r\n</Component>\r\n\r\n';
  const { input, output, env, result } = runModel(root, [modelEdit({line:4})], text);
  assert.equal(result.status, 0, result.stdout);
  assert.equal(readFileSync(input, 'utf8'), text);
  assert.equal(readFileSync(output, 'utf8'), text.replace('A bad sentence.', 'A good sentence.'));
  assert.doesNotMatch(readFileSync(env.FIXTURE_CALLS, 'utf8'), /secret_/);
  assert.equal(JSON.parse(result.stdout).coverage.protectedLines.length, 17);
}));

test('a model cannot edit protected code or change structural delimiters', () => {
  for (const [text, edit] of [['```\nbad\n```\nplain\n', modelEdit({line:2})], ['A bad sentence.', modelEdit({to:'**good**'})]]) fixture(root => {
    const { output, result } = runModel(root, [edit], text);
    assert.notEqual(result.status, 0); assert.equal(existsSync(output), false);
  });
});

test('source drift and late output creation refuse without clobbering foreign bytes', () => {
  for (const action of ['changeSource', 'lateOutput']) fixture(root => {
    const control = {[action]: join(root, action === 'changeSource' ? 'doc.md' : 'doc.proofread.md')};
    const { output, result } = runModel(root, [modelEdit()], undefined, control);
    assert.notEqual(result.status, 0);
    assert.equal(JSON.parse(result.stdout).error, action === 'changeSource' ? 'source_changed' : 'output_exists');
    assert.equal(action === 'changeSource' ? existsSync(output) : readFileSync(output, 'utf8'), action === 'changeSource' ? false : 'foreign');
    assert.equal(readdirSync(root).some(name => name.startsWith('.proofread-')), false);
  });
});

test('existing output and output symlink refuse before provider initialization', () => {
  for (const kind of ['file','symlink']) fixture(root => {
    const input = join(root, 'doc.md'); writeFileSync(input, 'text');
    const output = join(root, 'doc.proofread.md');
    const other = join(root, 'other'); writeFileSync(other, 'foreign');
    if (kind === 'symlink') symlinkSync(other, output); else writeFileSync(output, 'foreign');
    const result = cli('proofread.ts', [input]);
    assert.equal(JSON.parse(result.stdout).error, 'output_exists');
    assert.equal(readFileSync(other, 'utf8'), 'foreign');
    assert.equal(readFileSync(output, 'utf8'), 'foreign');
  });
});

test('strict arguments and invalid UTF8 fail before credentials or provider access', () => fixture(root => {
  const input = join(root, 'doc.md'); writeFileSync(input, Buffer.from([0xff]));
  for (const args of [[input,'--level','2junk'], [input,'--level','NaN'], [input,'--engine'], [input,'--unknown','x'], [input,'--language','german'], [input,'--level','1','--level','2']]) {
    assert.equal(JSON.parse(cli('proofread.ts', args).stdout).error, 'invalid_arguments');
  }
  assert.equal(JSON.parse(cli('proofread.ts', [input]).stdout).error, 'invalid_utf8');
}));

test('spellcheck honors language, quotes commands, and retains sentence-initial typos', () => fixture(root => {
  const input = join(root, 'doc.md'); writeFileSync(input, '!Sentnce here.  \r\n');
  const env = fakeAspell(root);
  const result = cli('proofread.ts', [input,'--engine','spellcheck','--language','american'], env);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  const report = JSON.parse(result.stdout);
  assert.equal(report.autoApplied.count, 0); assert.equal(report.suggestions.length, 1);
  assert.equal(report.suggestions[0].from, 'Sentnce');
  const call = JSON.parse(readFileSync(env.FIXTURE_ASPELL_CALLS, 'utf8'));
  assert.deepEqual(call.args, ['-a','--mode=none','--encoding=utf-8','--lang=en_US']); assert.equal(call.input, '^!Sentnce here.  \n');
}));

test('spellcheck subprocess failures are not ignored or leaked', () => fixture(root => {
  const input = join(root, 'doc.md'); writeFileSync(input, 'Sentnce.');
  const result = cli('proofread.ts', [input,'--engine','spellcheck'], fakeAspell(root,'error'));
  assert.equal(JSON.parse(result.stdout).error, 'spellcheck_failed');
  assert.doesNotMatch(result.stdout + result.stderr, /private-child-error/);
  assert.equal(existsSync(join(root,'doc.proofread.md')), false);
}));

test('MDX generation and all/none application preserve JSX, Unicode, CRLF and hard breaks', () => {
  for (const selection of ['all','none']) fixture(root => {
    const input = join(root, 'naïve doc.mdx');
    const text = 'Sentnce café.  \r\n\r\n<Component>\r\nSentnce protected\r\n</Component>\r\n';
    writeFileSync(input, text);
    const generated = cli('proofread.ts', [input,'--engine','spellcheck'], fakeAspell(root));
    assert.equal(generated.status, 0, generated.stdout);
    const reviewed = join(root,'naïve doc.proofread.mdx');
    assert.match(readFileSync(reviewed,'utf8'), /\{\/\* proofread:/);
    assert.doesNotMatch(readFileSync(reviewed,'utf8'), /<!--/);
    const result = cli('apply-suggestions.ts', [reviewed,selection]);
    assert.equal(result.status, 0, result.stdout);
    assert.equal(readFileSync(join(root,'naïve doc.final.mdx'),'utf8'), selection === 'none' ? text : text.replace('Sentnce café.','Sentence café.'));
    assert.equal(readFileSync(input,'utf8'), text);
  });
});

test('duplicate/malformed selection and markers, stale lines and legacy acceptance refuse', () => {
  const cases = [
    [marker('old text'), ['S1','S1']], [marker('old text'), ['all','S1']], [marker('old text'), ['none','S1']],
    [marker('old text'), ['S0']], [marker('old text')+' '+marker('old text'), ['all']],
    ['old text <!-- proofread:invalid -->', ['none']], [marker('old text').replace('old text ', 'changed text '), ['all']],
    [marker('old text','old','new',false,{version:undefined}), ['all']],
    ['old text <!-- [S1] REVIEW: Old format -->', ['all']],
    [marker('old old text'), ['all']], [marker('old text','absent','new'), ['all']],
  ];
  for (const [text, selection] of cases) fixture(root => {
    const input = join(root,'doc.proofread.md'); writeFileSync(input,text);
    const result = cli('apply-suggestions.ts',[input,...selection]);
    assert.notEqual(result.status,0,text); assert.equal(existsSync(join(root,'doc.final.md')),false);
    assert.equal(readFileSync(input,'utf8'),text);
  });
});

test('legacy markers may be explicitly discarded with whitespace intact', () => fixture(root => {
  const input = join(root,'doc.proofread.md');
  writeFileSync(input,marker('old text  ','old','new',false,{version:undefined})+'\r\n');
  const result = cli('apply-suggestions.ts',[input,'none']);
  assert.equal(result.status,0,result.stdout);
  assert.equal(readFileSync(join(root,'doc.final.md'),'utf8'),'old text  \r\n');
  assert.deepEqual(JSON.parse(result.stdout).removed,['S1']);
}));

test('overlapping edits refuse and independent edits use original literal coordinates', () => {
  assert.throws(() => applyEdits(splitLines('abcdef'), [{line:1,from:'abc',to:'x'},{line:1,from:'bc',to:'y'}]), error => error.code === 'overlapping_edits');
  assert.equal(applyEdits(splitLines('old other'), [{line:1,from:'old',to:'other'},{line:1,from:'other',to:'new'}])[0].body,'other new');
});

test('publication checks source replacement and leaves unrelated output untouched', () => fixture(root => {
  const input = join(root,'doc.md'); writeFileSync(input,'text');
  const snapshot = readSnapshot(input); const output = join(root,'doc.proofread.md');
  rmSync(input); writeFileSync(input,'text');
  assert.throws(() => publish(snapshot,output,'changed'), error => error.code === 'source_changed');
  assert.equal(existsSync(output),false);
}));

test('non-normal provider termination never certifies an empty successful review', () => {
  for (const finishReason of ['length','content-filter','tool-calls','error','other',null]) fixture(root => {
    const { output, result } = runModel(root, [], undefined, {finishReason});
    assert.notEqual(result.status,0); assert.equal(JSON.parse(result.stdout).error,'provider_incomplete');
    assert.equal(existsSync(output),false);
  });
});

test('equal delimiter counts and whitespace edits cannot change Markdown structure', () => {
  for (const [text, from, to] of [['**bad** sentence','**bad**','***bad*'], ['bad sentence  ','sentence  ','sentence'], ['bad sentence','bad','    good']]) fixture(root => {
    const { output, result } = runModel(root, [modelEdit({from,to})], text);
    assert.equal(JSON.parse(result.stdout).error,'structural_edit'); assert.equal(existsSync(output),false);
  });
});

test('overlapping correction and suggestion cannot relocate into the corrected substring', () => fixture(root => {
  const { output, result } = runModel(root, [modelEdit({to:'badder'}), modelEdit({type:'suggestion',to:'poor'})]);
  assert.equal(JSON.parse(result.stdout).error,'overlapping_edits'); assert.equal(existsSync(output),false);
}));

test('atomic publication preserves a target created after the final precheck', () => fixture(root => {
  const { output, result } = runModel(root, [modelEdit()], undefined, {lateLink:true});
  assert.equal(JSON.parse(result.stdout).error,'output_exists');
  assert.equal(readFileSync(output,'utf8'),'foreign-at-link');
  assert.equal(readdirSync(root).some(name => name.startsWith('.proofread-')),false);
}));

test('container fences and multiple comment delimiters protect their full bodies', () => {
  for (const text of ['- ```text\n  Sentnce\n  ```\n', '1. ```text\n   Sentnce\n   ```\n', '> ~~~text\n> Sentnce\n> ~~~\n', '<!-- closed --> <!--\nSentnce\n-->\n']) fixture(root => {
    const input = join(root,'doc.md'); writeFileSync(input,text);
    const env = fakeAspell(root);
    const result = cli('proofread.ts',[input,'--engine','spellcheck'],env);
    assert.equal(result.status,0,result.stdout);
    assert.equal(JSON.parse(result.stdout).suggestions.length,0);
    assert.equal(existsSync(env.FIXTURE_ASPELL_CALLS),false);
    const output = join(root,'doc.proofread.md');
    assert.equal(readFileSync(output,'utf8'),text);
    assert.equal(cli('apply-suggestions.ts',[output,'none']).status,0);
    assert.equal(readFileSync(join(root,'doc.final.md'),'utf8'),text);
  });
});

test('terminal stop cannot hide actual or malformed tool calls', () => {
  for (const toolCalls of [[{type:'tool-call'}], null, {}]) fixture(root => {
    const { output, result } = runModel(root, [], undefined, {toolCalls});
    assert.equal(JSON.parse(result.stdout).error,'provider_incomplete'); assert.equal(existsSync(output),false);
  });
});

test('display math and uncertain raw markup remain protected across blank lines', () => {
  for (const text of ['$$\n\nSentnce\n$$\n', '\\[\nSentnce\n\\]\n', '\\begin{equation}\nSentnce\n\\end{equation}\n', '<pre>\n\nSentnce\n</pre>\n', '{\n\nSentnce\n}\n']) fixture(root => {
    const input = join(root,'doc.mdx'); writeFileSync(input,text);
    const env = fakeAspell(root);
    const result = cli('proofread.ts',[input,'--engine','spellcheck'],env);
    assert.equal(result.status,0,result.stdout); assert.equal(existsSync(env.FIXTURE_ASPELL_CALLS),false);
    assert.equal(readFileSync(join(root,'doc.proofread.mdx'),'utf8'),text);
  });
});

test('complete one-line MDX imports and elements do not suppress following prose', () => {
  for (const prefix of ["import Thing from './component';", "import { Thing } from './component';", '<Widget />', '<Widget>Text</Widget>']) fixture(root => {
    const input = join(root,'doc.mdx'); writeFileSync(input,prefix+'\r\n\r\nSentnce café.  \r\n');
    const generated = cli('proofread.ts',[input,'--engine','spellcheck'],fakeAspell(root));
    assert.equal(generated.status,0,generated.stdout); assert.equal(JSON.parse(generated.stdout).suggestions.length,1);
    const reviewed = join(root,'doc.proofread.mdx');
    assert.equal(cli('apply-suggestions.ts',[reviewed,'all']).status,0);
    assert.equal(readFileSync(join(root,'doc.final.mdx'),'utf8'),prefix+'\r\n\r\nSentence café.  \r\n');
  });
});
