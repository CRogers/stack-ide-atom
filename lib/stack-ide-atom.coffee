AtomStackIdeView = require './stack-ide-atom-view'
{CompositeDisposable} = require 'atom'

haskell = require('./haskell/generated/haskell')
p = haskell.getPackage()
console.log p

module.exports = AtomStackIde =
  atomStackIdeView: null
  modalPanel: null
  subscriptions: null

  activate: (state) ->
    @atomStackIdeView = new AtomStackIdeView(state.atomStackIdeViewState)
    @modalPanel = atom.workspace.addModalPanel(item: @atomStackIdeView.getElement(), visible: false)

    # Events subscribed to in atom's system can be easily cleaned up with a CompositeDisposable
    @subscriptions = new CompositeDisposable

    # Register command that toggles this view
    @subscriptions.add atom.commands.add 'atom-text-editor',
      'stack-ide-atom:source-errors': (event) ->
        editor = @getModel()
        console.log editor.getText()

  deactivate: ->
    @modalPanel.destroy()
    @subscriptions.dispose()
    @atomStackIdeView.destroy()

  serialize: ->
    atomStackIdeViewState: @atomStackIdeView.serialize()

  toggle: ->
    console.log 'AtomStackIde was toggled!'

    if @modalPanel.isVisible()
      @modalPanel.hide()
    else
      @modalPanel.show()
