{Range} = require('atom')
_ = require 'lodash'
process = require 'process'

# Use the command `window:run-package-specs` (cmd-alt-ctrl-p) to run specs.
#
# To run a specific `it` or `describe` block add an `f` to the front (e.g. `fit`
# or `fdescribe`). Remove the `f` to unfocus the block.

describe 'AtomStackIde', ->
  [workspaceElement, activationPromise, textEditor, textEditorElement] = []

  beforeEach ->
    workspaceElement = atom.views.getView(atom.workspace)
    activationPromise = atom.packages.activatePackage('stack-ide-atom')

    waitsForPromise ->
      activationPromise

  afterEach ->
    for x in [0..100]
      textEditor.undo()
    textEditor.save()

  loadProject = (projectPath, file) ->
    absoluteProjectPath = null

    runs ->
      packagePath = atom.packages.getLoadedPackage('stack-ide-atom').path
      absoluteProjectPath = "#{packagePath}/#{projectPath}"
      atom.project.setPaths([absoluteProjectPath])

    waitsForPromise ->
      atom.workspace.open("#{absoluteProjectPath}/OneSourceError.hs")
        .then (editor) -> textEditor = editor

    runs ->
      textEditorElement = atom.views.getView(textEditor)

  loadOneSourceErrorProject = ->
    loadProject('haskell/test-data/one-source-error', 'OneSourceError.hs')

  loadTwoSourceErrorsProject = ->
    loadProject('haskell/test-data/two-source-errors', 'TwoSourceErrors.hs')

  dispatchSourceErrorsCommand = ->
    runs ->
      atom.commands.dispatch(textEditorElement, 'stack-ide-atom:source-errors')

  getDecorations = ->
    _.filter textEditor.getDecorations(), (decoration) ->
      decoration.properties.type == 'highlight' and
      decoration.properties.class == 'sia-error'

  waitForDecorations = (num) ->
    waitsFor ->
      return getDecorations().length == num

  initialErrorRange = new Range([3,7], [3,8])

  describe 'Source Errors', ->
    it 'should create a decoration when there is a source error', ->
      loadOneSourceErrorProject()
      dispatchSourceErrorsCommand()
      waitForDecorations(1)

      runs ->
        decorations = getDecorations()
        expect(decorations.length).toBe 1
        correctRange = decorations[0].getMarker().getBufferRange().isEqual(initialErrorRange)
        expect(correctRange).toBe true

    it 'should clear old decorations when the error is corrected', ->
      loadOneSourceErrorProject()
      dispatchSourceErrorsCommand()
      waitForDecorations(1)

      runs ->
        textEditor.setTextInBufferRange(initialErrorRange, '"foo"')
        textEditor.save()

      dispatchSourceErrorsCommand()
      waitForDecorations(0)

    it 'should create two decorations when there are two source errors', ->
      loadTwoSourceErrorsProject()
      dispatchSourceErrorsCommand()
      waitForDecorations(2)

  describe 'Error Handling', ->
    PATH = null
    beforeEach ->
      PATH = process.env.PATH

    afterEach ->
      process.env.PATH = PATH

    it 'should put up an error if the path does not contain the stack executable', ->
      process.env.PATH = ''
      loadOneSourceErrorProject()
      dispatchSourceErrorsCommand()

      waitsFor ->
        return atom.notifications.getNotifications().length == 1

