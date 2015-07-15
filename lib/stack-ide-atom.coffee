AtomStackIdeView = require './stack-ide-atom-view'
{CompositeDisposable} = require 'atom'

haskell = require('./haskell/generated/haskell')
haskell.main()

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
    @subscriptions.add atom.commands.add 'atom-workspace', 'stack-ide-atom:toggle': => @toggle()

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
