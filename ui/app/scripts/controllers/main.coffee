'use strict'

angular.module('uiApp')
  .controller 'MainCtrl', ($scope, Channel) ->

    $scope.systemConfig = {}

    Channel.connected().then (ch) ->
      ch.send({command: "info"}).then (resp) ->
        $scope.systemConfig = resp.config
