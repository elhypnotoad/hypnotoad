'use strict'

describe 'Directive: activeLink', () ->

  # load the directive's module
  beforeEach module 'uiApp'

  scope = {}

  beforeEach inject ($controller, $rootScope) ->
    scope = $rootScope.$new()

  it 'should make hidden element visible', inject ($compile) ->
    element = angular.element '<active-link></active-link>'
    element = $compile(element) scope
    expect(element.text()).toBe 'this is the activeLink directive'
