var React = require('react')
var Nav = require('../views/nav.jsx')
var TabbedArea = require('react-bootstrap/TabbedArea')
var TabPane = require('react-bootstrap/TabPane')

module.exports = React.createClass({
  render: function() {
    return (
  <div className="row">
    <div className="col-sm-8 col-sm-offset-2">

      <Nav activeKey={1} />

    </div>
  </div>
    )
  }
})
