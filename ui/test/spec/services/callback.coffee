'use strict'

describe 'Service: Callback', () ->

  # load the service's module
  beforeEach module 'uiApp'

  # instantiate service
  Callback = {}
  beforeEach inject (_Callback_) ->
    Callback = _Callback_

  it 'should do something', () ->
    expect(!!Callback).toBe true
