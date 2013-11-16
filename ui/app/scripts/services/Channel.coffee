'use strict'

angular.module('uiApp')
  .factory 'Channel', ($q, $callback, $rootScope, Config) ->
    Channel = {}

    ChannelObject = (socket) ->
      @socket = socket
      me = this
      @socket.onmessage = (e) ->
        me.onMessage(e)
      @matrix = {}
      this

    ChannelObject::send = (data) ->
      dfd = $callback.defer()
      id = _.uniqueId()
      @matrix[id] = dfd
      payload = angular.extend(data, id: id)
      @socket.send(angular.toJson(payload))
      dfd.promise

    ChannelObject::onMessage = (e) ->
      packet = angular.fromJson(e.data)
      id = packet.id
      matrix = @matrix
      $rootScope.$apply () ->
        matrix[id].notify(packet)

    channelDeferred = $q.defer()
    Channel.connected = () ->
      channelDeferred.promise

    getConfig = () ->
      dfd = $q.defer()

      Config.get().then (data) ->
        dfd.resolve(data.api_server)

      dfd.promise

    connect = (url) ->
      socket = new WebSocket(url)
      socket.onopen = () -> 
        channel = new ChannelObject(socket)
        channelDeferred.resolve(channel)

    getConfig().then (url) ->
      connect(url)

    Channel
