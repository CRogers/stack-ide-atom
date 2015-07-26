{Range} = require('atom')
_ = require 'lodash'

# Use the command `window:run-package-specs` (cmd-alt-ctrl-p) to run specs.
#
# To run a specific `it` or `describe` block add an `f` to the front (e.g. `fit`
# or `fdescribe`). Remove the `f` to unfocus the block.

describe 'AtomStackIde', ->
  [workspaceElement, activationPromise, oneSourceErrorPath, textEditor, textEditorElement] = []

  beforeEach ->
    workspaceElement = atom.views.getView(atom.workspace)
    activationPromise = atom.packages.activatePackage('stack-ide-atom')

    waitsForPromise ->
      activationPromise

    runs ->
      packagePath = atom.packages.getLoadedPackage('stack-ide-atom').path
      oneSourceErrorPath = "#{packagePath}/haskell/test-data/one-source-error"
      atom.project.setPaths([oneSourceErrorPath])

    waitsForPromise ->
      atom.workspace.open("#{oneSourceErrorPath}/OneSourceError.hs")
        .then (editor) -> textEditor = editor

    runs ->
      textEditorElement = atom.views.getView(textEditor)

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
      dispatchSourceErrorsCommand()
      waitForDecorations(1)

      runs ->
        decorations = getDecorations()
        expect(decorations.length).toBe 1
        correctRange = decorations[0].getMarker().getBufferRange().isEqual(initialErrorRange)
        expect(correctRange).toBe true

    it 'should clear old decorations when the error is corrected', ->
      dispatchSourceErrorsCommand()
      waitForDecorations(1)

      runs ->
        textEditor.setTextInRange(initialErrorRange, '"foo"')
        console.log textEditor.getText()

      dispatchSourceErrorsCommand()
      waitForDecorations(0)


