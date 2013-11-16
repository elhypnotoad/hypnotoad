'use strict'

describe 'Service: Channel', () ->

  # load the service's module
  beforeEach module 'uiApp'

  # instantiate service
  Channel = {}
  beforeEach inject (_Channel_) ->
    Channel = _Channel_

  it 'should do something', () ->
    expect(!!Channel).toBe true
