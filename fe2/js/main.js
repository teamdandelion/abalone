var React = require('react')
var Router = require('react-router')
var Routes = require('./app.jsx')

// jquery entry point.
$(document).ready(function() {
  var appEl = document.getElementById('app');
  Router.run(Routes, Router.HistoryLocation, function (Handler) {
      React.render(React.createElement(Handler), appEl);
  });
})
