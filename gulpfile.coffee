path = require('path')

gulp = require('gulp')
shell = require('gulp-shell')

haskellCmds = (cmds) ->
  shell.task(cmds, {cwd: 'haskell'})

gulp.task 'build-haskell', [], haskellCmds([
  'stack build --install-ghc'
])

gulp.task 'build-stack-ide', shell.task([
  'stack install --install-ghc'
], {cwd: 'stack-ide'})

hspecLoc = 'dist/build/hspec/hspec'

gulp.task 'test', ['build-haskell', 'build-stack-ide'], haskellCmds([
  "echo 'function h$putenv(){}' >>#{hspecLoc}"
  "node #{hspecLoc}"
])

GENERATED_JS_FILES = ['rts.js','lib.js','out.js'].join(' ')
GENERATED_HASKELL_DIR = path.resolve('./lib/haskell/generated')

gulp.task 'copy-generated-js', ['build-haskell'], shell.task([
  "cp #{GENERATED_JS_FILES} #{GENERATED_HASKELL_DIR}"
], {cwd: '.stack-work/install/x86_64-osx/ghcjs-0.1.0.20150924_ghc-7.10.2/ghcjs-0.1.0.20150924_ghc-7.10.2/bin/stack-ide-atom.jsexe'})

gulp.task 'build', ['copy-generated-js'], shell.task([
  "cat ../module-start.js #{GENERATED_JS_FILES} ../module-end.js >haskell.js"
  "rm #{GENERATED_JS_FILES}"
], {cwd: GENERATED_HASKELL_DIR})

gulp.task 'watch-test', ->
  gulp.watch 'haskell/**/*.hs', ['test']

gulp.task 'update-subtrees', shell.task([
  'git subtree pull --squash --prefix=stack-ide https://github.com/commercialhaskell/stack-ide.git master'
  'git subtree pull --squash --prefix=ide-backend https://github.com/fpco/ide-backend.git master'
])

gulp.task 'circleci', ['test']

gulp.task 'clear-sessions', haskellCmds([
  'rm -r test-data/*/session.*'
])