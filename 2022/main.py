import os
import platform

input_file = 'input.txt'

def main():
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
		print('Day '+dir+':')
		os.popen(ghc_command).read()
		os.system(run_command)

if __name__ == '__main__':
	main()