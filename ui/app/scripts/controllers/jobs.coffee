'use strict'

angular.module('uiApp')
  .controller 'JobsCtrl', ($scope, $location, $routeParams, Channel) ->

    $scope.jobs = []
    $scope.plan = $routeParams.plan
    $scope.filter = $location.search().q

    Channel.connected().then (ch) ->
      ch.send({command: "plans"}).then (resp) ->
      	$scope.planObject = _.find(resp.plans,(p) -> p.name == $scope.plan)

    $scope.showLog = (i) ->
      $scope.jobs[i].showLog = not $scope.jobs[i].showLog

    $scope.showReqs = (i) ->
      $scope.jobs[i].showReqs = not $scope.jobs[i].showReqs

    Channel.connected().then (ch) ->
      ch.send({command: "jobs", plan: $scope.plan }).then (resp) ->
       jobs = _.map(resp.jobs, (job, i) -> angular.extend(job, {optionsText: angular.toJson(job.options), showLog: _.find($scope.jobs, (j) -> j.id == job.id)?.showLog, showReqs: _.find($scope.jobs, (j) -> j.id == job.id)?.showReqs}))
       $scope.jobs = jobs

    $scope.identity = (job) ->
      job.id

    $scope.match = (job, filter) ->
      (job.module.match(RegExp(filter,'i')) != null) or
      (job.status.match(RegExp(filter,'i')) != null) or
      (job.host.match(RegExp(filter,'i')) != null) or
      (job.output.match(RegExp(filter,'i')) != null) or
      (job.optionsText.match(RegExp(filter,'i')) != null)