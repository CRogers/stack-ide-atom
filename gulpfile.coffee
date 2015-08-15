path = require('path')

gulp = require('gulp')
shell = require('gulp-shell')

haskellCmds = (cmds) ->
  shell.task(cmds, {cwd: 'haskell'})

gulp.task 'create-sandbox', haskellCmds([
  'cabal sandbox init'
  'cabal sandbox add-source ../stack-ide/ide-backend/ide-backend-common'
  'cabal sandbox add-source ../stack-ide/stack-ide-api'
])

gulp.task 'install-deps', ['create-sandbox'], haskellCmds([
  'cabal update'
  'cabal install --only-dependencies --enable-tests --ghcjs'
])

gulp.task 'configure-haskell', ['install-deps'], haskellCmds([
  'cabal configure --enable-tests --ghcjs'
])

gulp.task 'build-haskell', [], haskellCmds([
  'cabal build'
])

gulp.task 'build-stack-ide', shell.task([
  'stack install stack-ide stack-ide-api ../ide-backend/ide-backend ../ide-backend/ide-backend-server ../ide-backend/ide-backend-common'
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
], {cwd: 'haskell/dist/build/stack-ide-atom/stack-ide-atom.jsexe'})

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