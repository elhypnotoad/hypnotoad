'use strict'

describe 'Controller: JobsCtrl', () ->

  # load the controller's module
  beforeEach module 'uiApp'

  JobsCtrl = {}
  scope = {}

  # Initialize the controller and a mock scope
  beforeEach inject ($controller, $rootScope) ->
    scope = $rootScope.$new()
    JobsCtrl = $controller 'JobsCtrl', {
      $scope: scope
    }

  it 'should attach a list of awesomeThings to the scope', () ->
    expect(scope.awesomeThings.length).toBe 3
