Atom = require('atom')
AtomStackIde = require '../lib/stack-ide-atom'
_ = require 'lodash'

# Use the command `window:run-package-specs` (cmd-alt-ctrl-p) to run specs.
#
# To run a specific `it` or `describe` block add an `f` to the front (e.g. `fit`
# or `fdescribe`). Remove the `f` to unfocus the block.

describe "AtomStackIde", ->
  [workspaceElement, activationPromise] = []

  beforeEach ->
    workspaceElement = atom.views.getView(atom.workspace)
    activationPromise = atom.packages.activatePackage('stack-ide-atom')

  describe "Source Errors", ->
    it "should create a decoration when there is a source error", ->
      waitsForPromise ->
        activationPromise

      oneSourceErrorPath = null

      runs ->
        packagePath = atom.packages.getLoadedPackage('stack-ide-atom').path
        oneSourceErrorPath = "#{packagePath}/haskell/test-data/one-source-error"
        atom.project.setPaths([oneSourceErrorPath])

      textEditor = null

      waitsForPromise ->
        atom.workspace.open("#{oneSourceErrorPath}/OneSourceError.hs")
          .then (editor) -> textEditor = editor

      runs ->
        textEditorElement = atom.views.getView(textEditor)
        atom.commands.dispatch(textEditorElement, 'stack-ide-atom:source-errors')

      decorations = null

      waitsFor ->
        decorations = _.filter textEditor.getDecorations(), (decoration) ->
          decoration.properties.type == 'highlight' and
          decoration.properties.class == 'sia-error'

        return decorations.length > 0

      runs ->
        expect(decorations.length).toBe 1
        correctRange = decorations[0].getMarker().getBufferRange().isEqual(new Atom.Range([3,7], [3,8]))
        expect(correctRange).toBe true
