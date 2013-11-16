'use strict'

angular.module('uiApp')
  .controller 'HostsCtrl', ($scope, Channel) ->

    $scope.hosts = []

    Channel.connected().then (ch) ->
      ch.send({command: "hosts"}).then (resp) ->
        hosts = _.map(resp.hosts, (host, i) -> angular.extend(host, {showFacts: _.find($scope.hosts, (h) -> h.name is host.name and _.isEqual(h.value, host.value))?.showFacts}))
        $scope.hosts = hosts

    $scope.showFacts = (i) ->
      $scope.hosts[i].showFacts = not $scope.hosts[i].showFacts
