'use strict'

describe 'Controller: HostsCtrl', () ->

  # load the controller's module
  beforeEach module 'uiApp'

  HostsCtrl = {}
  scope = {}

  # Initialize the controller and a mock scope
  beforeEach inject ($controller, $rootScope) ->
    scope = $rootScope.$new()
    HostsCtrl = $controller 'HostsCtrl', {
      $scope: scope
    }

  it 'should attach a list of awesomeThings to the scope', () ->
    expect(scope.awesomeThings.length).toBe 3
