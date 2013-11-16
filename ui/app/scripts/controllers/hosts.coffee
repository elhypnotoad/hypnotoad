'use strict'

angular.module('uiApp')
  .controller 'HostsCtrl', ($scope, Channel) ->

    $scope.hosts = []

    Channel.connected().then (ch) ->
      ch.send({command: "hosts"}).then (resp) ->
        $scope.hosts = resp.hosts