define('gmaps',['async!https://maps.googleapis.com/maps/api/js?key=AIzaSyDwIEnn4FJ7pxowioYuB_OI0FzxcNVlXV0'], 
  function() {
    return window.google.maps;
  }
);

define('map', ['gmaps','config'],
  function(gmaps, config) {
    gmaps.visualRefresh = true;
    var map = new gmaps.Map(document.getElementById('map-canvas'), {
      center: new gmaps.LatLng(config.INITIAL_LAT_POSITION, 
                               config.INITIAL_LNG_POSITION),
      zoom: config.GOOGLE_MAPS_DEFAULT_ZOOM,
      minZoom: config.google_maps_min_zoom,
      mapTypeId: gmaps.MapTypeId.ROADMAP,
      streetViewControl: config.GOOGLE_MAPS_STREET_VIEW_CONTROL
    });
    var tl = new gmaps.TrafficLayer();
    tl.setMap(map);
    return map;
  });

define(['jquery','gmaps','map','config'],
  function($, gmaps, map, config) {
    var ws;
    var inter = null;
    function connect() {
      ws = new WebSocket('ws://'+window.location.host+'/websocket');
      ws.onopen = function(event) {
        console.log('connected');
        if (inter) {
          clearInterval(inter);
        }
        inter = setInterval(function() {ws.send('ping!');},3000);
      };
      ws.onclose = function(event) {
        console.log('disconnected');
        ws.close();
        setTimeout(function() {connect();}, 1000);
      };
      ws.onmessage = function(event) {
        if (event.data != 'pong!') {
          actualizar(JSON.parse(event.data));
        }
      };
      ws.onerror = function(event) {
        console.log(event);
      };
    }
    connect();
    var buses = Object.create(null);

    function actualizar(data) {
      var unit = data.unit;
      var route = data.route;
      var plate = data.plate;
      var url = data.url;
      var ok = parseInt(data.ok);
      var lat = parseFloat(data.lat);
      var lng = parseFloat(data.lng);
      var gpsLatLng = new gmaps.LatLng(lat,lng);
      var title = 'Unidad: "'+unit+'", ruta: "'+route+'", placa: "'+plate+'"';
      if (unit in buses) {
	      var bus = buses[unit];
        bus.setPosition(gpsLatLng);
        bus.setTitle(title);
	      if (ok == 0 && bus.ok == 1) {
          map.setCenter(bus.getPosition());
          map.setZoom(config.GOOGLE_MAPS_ATENDING_ZOOM);
        }
        bus.ok = ok;
        if (bus.ok == 0) {
          bus.setIcon('http://maps.google.com/mapfiles/kml/paddle/stop.png');
        } else {
          bus.setIcon('http://maps.google.com/mapfiles/ms/icons/green.png');
        }
      } else {
        var bus = new gmaps.Marker({
          map: map,
          position: gpsLatLng,
          icon: 'http://maps.google.com/mapfiles/ms/icons/green.png',
          title: title,
        });
        bus.addListener('click', function() {
          window.open(url, '_blank','location=no');
        });
        bus.ok = 1;
        buses[unit] = bus;
      }
    }
  });

