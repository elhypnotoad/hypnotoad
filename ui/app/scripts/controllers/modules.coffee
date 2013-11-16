'use strict'

angular.module('uiApp')
  .controller 'ModulesCtrl', ($scope, $location, Channel) ->

    $scope.modules = []

    Channel.connected().then (ch) ->
      ch.send({command: "modules"}).then (resp) ->
        $scope.modules = resp.modules

    $scope.reload = ->
      Channel.connected().then (ch) ->
        ch.send({command: "reload_modules"})
