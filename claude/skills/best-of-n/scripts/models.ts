/**
 * Model definitions and Vercel AI SDK provider setup
 * Model definitions, Vercel AI SDK provider setup, and eligibility filtering for best-of-n
 */

import { openai } from '@ai-sdk/openai';
import { google } from '@ai-sdk/google';
import { xai } from '@ai-sdk/xai';
import { createAnthropic } from '@ai-sdk/anthropic';
import type { LanguageModel } from 'ai';
import { readFileSync, existsSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const BON_DIR = join(__dirname, '..');

export interface ModelConfig {
  provider: 'openai' | 'google' | 'xai' | 'anthropic' | 'openai-deep' | 'gemini-deep';
  display_name?: string;
  reasoning?: boolean;
  model_id: string;
  type: 'api' | 'browser';
  max_tokens?: number;
  requires_browser?: boolean;
  async?: boolean;
  url?: string;
  mode?: string;
  typical_duration_minutes?: number;
  slow?: boolean;
  timeout_seconds?: number;
  deep_research?: boolean;
  poll_interval_ms?: number;
  web_search?: boolean;
}

export interface PresetConfig {
  description: string;
  models: string[];
  timeout_seconds?: number;
  async?: boolean;
  requires_browser?: boolean;
  brainstorm?: boolean;
  num_samples?: number;
  temperature?: number;
  temperature_range?: [number, number];
}

export interface Config {
  presets: Record<string, PresetConfig>;
  models: Record<string, ModelConfig>;
  defaults: {
    preset: string;
    synthesis_depth: string;
    max_tokens: number;
  };
  synthesis_depths: Record<string, string>;
}

/**
 * Load config: models.json (shipped) + optional config.json (user overrides)
 */
export function loadConfig(): Config {
  const modelsPath = join(BON_DIR, 'models.json');
  const base: Config = JSON.parse(readFileSync(modelsPath, 'utf-8'));

  const overridePath = join(BON_DIR, 'config.json');
  if (existsSync(overridePath)) {
    const overrides = JSON.parse(readFileSync(overridePath, 'utf-8'));
    return {
      models: { ...base.models, ...overrides.models },
      presets: { ...base.presets, ...overrides.presets },
      defaults: { ...base.defaults, ...overrides.defaults },
      synthesis_depths: { ...base.synthesis_depths, ...overrides.synthesis_depths },
    };
  }

  return base;
}

/**
 * Create a Vercel AI SDK model instance from config
 */
export function createModel(modelName: string, config: Config): LanguageModel | null {
  const modelConfig = config.models[modelName];

  if (!modelConfig) {
    console.error(`Unknown model: ${modelName}`);
    return null;
  }

  if (modelConfig.type === 'browser') {
    return null;
  }

  switch (modelConfig.provider) {
    case 'openai':
      return openai.responses(modelConfig.model_id);
    case 'google':
      return google(modelConfig.model_id);
    case 'xai':
      return xai.responses(modelConfig.model_id);
    case 'anthropic': {
      const anthropicProvider = createAnthropic({
        baseURL: 'https://api.anthropic.com/v1',
      });
      return anthropicProvider(modelConfig.model_id);
    }
    case 'openai-deep':
    case 'gemini-deep':
      return null;
    default:
      console.error(`Unknown provider: ${(modelConfig as ModelConfig).provider}`);
      return null;
  }
}

/**
 * Get models eligible for best-of-n sampling.
 * Excludes: deep research, browser-only, and optionally slow models.
 */
export function getEligibleModels(config: Config): string[] {
  return Object.entries(config.models)
    .filter(([, mc]) => {
      if (mc.type === 'browser' || mc.requires_browser) return false;
      if (mc.deep_research) return false;
      if (mc.provider === 'openai-deep' || mc.provider === 'gemini-deep') return false;
      return true;
    })
    .map(([name]) => name);
}

/**
 * Get models for a preset, filtering to eligible-only
 */
export function getPresetModels(presetName: string, config: Config): string[] {
  const preset = config.presets[presetName];
  if (!preset) {
    throw new Error(`Unknown preset: ${presetName}`);
  }

  const eligible = new Set(getEligibleModels(config));
  return preset.models.filter(name => eligible.has(name));
}

/**
 * List available presets (only showing eligible models)
 */
export function listPresets(config: Config): void {
  console.log('\nAvailable presets:\n');
  for (const [name, preset] of Object.entries(config.presets)) {
    const models = getPresetModels(name, config);
    if (models.length === 0) continue; // skip presets with no eligible models
    console.log(`  ${name}`);
    console.log(`    ${preset.description}`);
    console.log(`    Models: ${models.join(', ')}`);
    if (preset.brainstorm) {
      const n = preset.num_samples ?? 4;
      const tempStr = preset.temperature_range
        ? `${preset.temperature_range[0]}→${preset.temperature_range[1]} (varies)`
        : String(preset.temperature ?? 0.8);
      console.log(`    Brainstorm: N=${n}, T=${tempStr}, ${models.length * n} API calls`);
    }
    console.log('');
  }
}

/**
 * List available models (eligible only)
 */
export function listModels(config: Config): void {
  console.log('\nEligible models for best-of-n:\n');
  const eligible = getEligibleModels(config);
  for (const name of eligible) {
    const mc = config.models[name];
    const slowNote = mc.slow ? ' (slow)' : '';
    console.log(`  ${name} (${mc.provider})${slowNote}`);
  }
}
