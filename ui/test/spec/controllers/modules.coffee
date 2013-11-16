'use strict'

describe 'Controller: ModulesCtrl', () ->

  # load the controller's module
  beforeEach module 'uiApp'

  ModulesCtrl = {}
  scope = {}

  # Initialize the controller and a mock scope
  beforeEach inject ($controller, $rootScope) ->
    scope = $rootScope.$new()
    ModulesCtrl = $controller 'ModulesCtrl', {
      $scope: scope
    }

  it 'should attach a list of awesomeThings to the scope', () ->
    expect(scope.awesomeThings.length).toBe 3
