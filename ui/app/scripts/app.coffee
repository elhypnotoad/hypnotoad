'use strict'

angular.module('uiApp', [
  'ngCookies',
  'ngResource',
  'ngSanitize',
  'ngRoute',
  'active-link'
])
  .config ($routeProvider) ->
    $routeProvider
      .when '/',
        templateUrl: 'views/main.html'
        controller: 'MainCtrl'
      .when '/hosts',
        templateUrl: 'views/hosts.html'
        controller: 'HostsCtrl'
      .when '/modules',
        templateUrl: 'views/modules.html'
        controller: 'ModulesCtrl'
      .when '/plans',
        templateUrl: 'views/plans.html'
        controller: 'PlansCtrl'
      .when '/plan/:plan/jobs',
        templateUrl: 'views/jobs.html'
        controller: 'JobsCtrl'
      .otherwise
        redirectTo: '/'
