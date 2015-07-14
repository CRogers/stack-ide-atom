gulp = require('gulp')
shell = require('gulp-shell')

haskellCmds = (cmds) ->
  shell.task(cmds, {cwd: haskell})

gulp.task 'create-sandbox', haskellCmds([
  'cabal sandbox init'
  'cabal sandbox add-source ../ide-backend/ide-backend-common'
  'cabal sandbox add-source ../stack-ide/stack-ide-api'
])

gulp.task 'install-deps', ['create-sandbox'], haskellCmds([
  'cabal install --only-dependencies --ghcjs'
])

gulp.task 'build', ['install-deps'], haskellCmds([
  'cabal configure --ghcjs'
  'cabal build'
])
