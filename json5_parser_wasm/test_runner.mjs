import { parseJson } from './pkg/nodejs/json5_parser_wasm.js';
import { readFileSync, readdirSync, statSync } from 'node:fs';
import { join } from 'node:path';

const succeeded = [];
const failed = [];

for (const testFile of findTestFiles()) {
	const content = readFileSync(testFile, { encoding: 'utf-8' });

	if (testFile.match(/\.(json|json5)$/i)) {
		try {
			parseJson(content);
			succeeded.push(testFile);
		} catch {
			console.log(`ERROR: ${testFile}`);
			failed.push(testFile);
		}
	} else {
		try {
			parseJson(content);
			failed.push(testFile);
		} catch {
			console.log(`OK:    ${testFile}`);
			succeeded.push(testFile);
		}
	}
}

succeeded.sort();
failed.sort();

for (const testFile of succeeded) {
	console.log(`OK:    ${testFile}`);
}
for (const testFile of failed) {
	console.log(`ERROR: ${testFile}`);
}

console.log();
console.log(`SUMMARY:
- Succeeded: ${succeeded.length}
- Failed:    ${failed.length}
- Total:     ${succeeded.length + failed.length}`);

function* findTestFiles(dir = './json5-tests') {
	for (const entry of readdirSync(dir, { encoding: 'utf-8' })) {
		if (entry.startsWith('.')) {
			/* Skip hidden entries (e.g. .git folder) */
			continue;
		}

		let stats = statSync(join(dir, entry));
		if (stats.isDirectory()) {
			yield* findTestFiles(join(dir, entry));
		} else if (stats.isFile() && !!entry.match(/\.(json|json5|js|txt)$/i)) {
			yield join(dir, entry);
		}
	}
}
