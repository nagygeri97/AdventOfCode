import os
import platform
import argparse
import time

input_file = 'input.txt'

def main():
	parser = argparse.ArgumentParser()
	parser.add_argument('-b', '--build-only', default=False, action='store_true', help='Only build solutions')
	parser.add_argument('-r', '--run-only', default=False, action='store_true', help='Only run solutions')
	parser.add_argument('-v', '--verbose', default=False, action='store_true', help='Show build and timing logs')
	args = parser.parse_args()
	if not args.build_only and not args.run_only:
		args.build_only = True
		args.run_only = True

	total_time = 0

	dirs = [dir for dir in os.listdir('.') if os.path.isdir(dir)]
	dirs.sort(key=int)

	ghc_command_template = 'ghc -O2 {{file}} {flags}'
	run_command_template = '{prefix}{{file}}{suffix} < {{input}}'

	isWin = platform.system() == 'Windows'
	if isWin:
		ghc_command_template = ghc_command_template.format(flags='')
		run_command_template = run_command_template.format(prefix='', suffix='.exe')
	else: 
		ghc_command_template = ghc_command_template.format(flags='-dynamic')
		run_command_template = run_command_template.format(prefix='./', suffix='')

	for dir in dirs:
		ghc_command = ghc_command_template.format(file=dir+'/main.hs').replace('/', '\\' if isWin else '/')
		run_command = run_command_template.format(file=dir+'/main', input=dir+'/'+input_file).replace('/', '\\' if isWin else '/')
		
		if args.verbose:
			print('Day '+dir+':')
		if args.build_only:
			os.popen(ghc_command).read()
			if args.verbose:
				print('Build complete')
		if args.run_only:
			if not args.verbose: # Still print the days when running the solutions, even if not verbose
				print('Day '+dir+':')
			day_start_time = time.time()
			os.system(run_command)
			day_end_time = time.time()
			day_time = day_end_time - day_start_time
			total_time += day_time
			if args.verbose:
				print('{:.4f} seconds'.format(day_time))
	if args.verbose and args.run_only: # Only print timing info when the solutions were run
		print('Total time: {:.4f} seconds'.format(total_time))


if __name__ == '__main__':
	main()