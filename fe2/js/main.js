var React = require('react')
var Router = require('react-router')
var Routes = require('./app.jsx')

// jquery entry point.
$(document).ready(function() {
  var appEl = document.getElementById('app');
  Router.run(Routes, Router.HistoryLocation, function (Handler, State) {
    var element = React.createElement(Handler);
    React.render(element, appEl);
  });
})
