'use strict'

angular.module('uiApp')
  .controller 'JobsCtrl', ($scope, $location, $routeParams, Channel) ->

    $scope.jobs = []
    $scope.plan = $routeParams.plan

    Channel.connected().then (ch) ->
      ch.send({command: "plans"}).then (resp) ->
      	$scope.planObject = _.find(resp.plans,(p) -> p.name == $scope.plan)

    $scope.showLog = (i) ->
      $scope.jobs[i].showLog = not $scope.jobs[i].showLog

    Channel.connected().then (ch) ->
      ch.send({command: "jobs", plan: $scope.plan }).then (resp) ->
      	jobs = _.map(resp.jobs, (job, i) -> angular.extend(job, {showLog: _.find($scope.jobs, (j) -> j.id == job.id)?.showLog}))
      	$scope.jobs = jobs

    $scope.identity = (job) ->
      job.id
