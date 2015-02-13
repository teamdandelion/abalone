var React = require('react')
var App = require('./app.jsx')
var linkHandler = require('./linkhandler.js')

// jquery entry point.
$(document).ready(function() {
  var appEl = document.getElementById('app')
  React.renderComponent(App(), appEl)
  linkHandler()
})
