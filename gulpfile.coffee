path = require('path')

gulp = require('gulp')
shell = require('gulp-shell')

haskellCmds = (cmds) ->
  shell.task(cmds, {cwd: 'haskell'})

gulp.task 'create-sandbox', haskellCmds([
  'cabal sandbox init'
  'cabal sandbox add-source ../ide-backend/ide-backend-common'
  'cabal sandbox add-source ../stack-ide/stack-ide-api'
])

gulp.task 'install-deps', ['create-sandbox'], haskellCmds([
  'cabal update'
  'cabal install --only-dependencies --enable-tests --ghcjs'
])

gulp.task 'configure-haskell', [], haskellCmds([
  'cabal configure --enable-tests --ghcjs'
])

gulp.task 'build-haskell', ['configure-haskell'], haskellCmds([
  'cabal build'
])

hspecLoc = 'dist/build/hspec/hspec'

gulp.task 'test', ['build-haskell'], haskellCmds([
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
