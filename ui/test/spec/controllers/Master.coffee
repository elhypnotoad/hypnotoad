'use strict'

describe 'Controller: MasterCtrl', () ->

  # load the controller's module
  beforeEach module 'uiApp'

  MasterCtrl = {}
  scope = {}

  # Initialize the controller and a mock scope
  beforeEach inject ($controller, $rootScope) ->
    scope = $rootScope.$new()
    MasterCtrl = $controller 'MasterCtrl', {
      $scope: scope
    }

  it 'should attach a list of awesomeThings to the scope', () ->
    expect(scope.awesomeThings.length).toBe 3
