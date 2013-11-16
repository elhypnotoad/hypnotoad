'use strict'

angular.module('uiApp')
  .controller 'MasterCtrl', ($scope, Config) ->
    Config.get().then (config) ->
      $scope.config = config