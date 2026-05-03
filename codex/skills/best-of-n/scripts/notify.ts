/**
 * Desktop notification wrapper using terminal-notifier (macOS)
 * Desktop notifications via terminal-notifier (macOS)
 */

import { execFileSync } from 'child_process';

function isTerminalNotifierInstalled(): boolean {
  try {
    execFileSync('which', ['terminal-notifier'], { stdio: 'ignore' });
    return true;
  } catch {
    return false;
  }
}

function notify(options: {
  title: string;
  message: string;
  subtitle?: string;
  sound?: string;
  open?: string;
  group?: string;
}): boolean {
  if (!isTerminalNotifierInstalled()) return false;

  const args: string[] = [
    '-title', options.title,
    '-message', options.message,
  ];

  if (options.subtitle) args.push('-subtitle', options.subtitle);
  if (options.sound) args.push('-sound', options.sound);
  if (options.open) args.push('-open', options.open);
  if (options.group) args.push('-group', options.group);

  try {
    execFileSync('terminal-notifier', args, { stdio: 'ignore' });
    return true;
  } catch {
    return false;
  }
}

export function notifyBonComplete(
  modelCount: number,
  samplesPerModel: number,
  outputPath: string,
): void {
  notify({
    title: 'Best-of-N',
    message: `${modelCount} models × ${samplesPerModel} samples complete`,
    subtitle: 'Query complete',
    sound: 'default',
    open: outputPath,
    group: 'best-of-n',
  });
}

export function notifyBonError(message: string): void {
  notify({
    title: 'Best-of-N — Error',
    message,
    sound: 'Basso',
    group: 'best-of-n-error',
  });
}
