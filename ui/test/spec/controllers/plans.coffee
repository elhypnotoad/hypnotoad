'use strict'

describe 'Controller: PlansCtrl', () ->

  # load the controller's module
  beforeEach module 'uiApp'

  PlansCtrl = {}
  scope = {}

  # Initialize the controller and a mock scope
  beforeEach inject ($controller, $rootScope) ->
    scope = $rootScope.$new()
    PlansCtrl = $controller 'PlansCtrl', {
      $scope: scope
    }

  it 'should attach a list of awesomeThings to the scope', () ->
    expect(scope.awesomeThings.length).toBe 3
