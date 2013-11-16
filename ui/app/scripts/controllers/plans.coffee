'use strict'

angular.module('uiApp')
  .controller 'PlansCtrl', ($scope, $location, Channel) ->

    $scope.plans = []

    Channel.connected().then (ch) ->
      ch.send({command: "plans"}).then (resp) ->
        $scope.plans = resp.plans

    $scope.showJobs = (name) ->
      $location.url("/plan/#{name}/jobs")

    $scope.run = (name) ->
      Channel.connected().then (ch) ->
        ch.send({command: "run_plan", plan: name}).then (resp) ->
          $scope.showJobs(name)

    $scope.reload = ->
      Channel.connected().then (ch) ->
        ch.send({command: "reload_modules"})
