var React = require('react')
var Nav = require('react-bootstrap/Nav')
var NavItem = require('react-bootstrap/NavItem')

module.exports = React.createClass({

  render: function() {
    return (
      <Nav bsStyle="tabs" activeKey={this.props.activeKey}  style={{"margin-bottom": "60px"}}>
        <NavItem key={1} href="/"><i className="fa fa-trophy"></i> Leaderboard</NavItem>
        <NavItem key={2} href="/play"><i className="fa fa-gamepad"></i> Play</NavItem>
        <NavItem key={3} href="/upload"><i className="fa fa-upload"></i> Upload a Player</NavItem>
      </Nav>
    )
  }
})
