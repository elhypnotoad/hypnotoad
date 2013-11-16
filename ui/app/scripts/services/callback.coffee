'use strict'

angular.module('uiApp')
  .factory '$callback', () ->
    defer: ->
        a = ->
          me = this
          me.callbacks = []
          me.notify = (value) ->
            me.notified = value
            me.callbacks.forEach (cb) ->
              cb(value)
          notify: me.notify
          promise:
            then: (cb) ->
              me.callbacks.push(cb)
              unless _.isUndefined(me.notified)
                me.notify(me.notified)
              me.promise
        new a()