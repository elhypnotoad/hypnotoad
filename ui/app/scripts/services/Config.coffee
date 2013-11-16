'use strict'

angular.module('uiApp')
  .factory 'Config', ($http) ->
    {
      get: () ->
        $http.get('config.json').then (data) ->
          data.data
    }
