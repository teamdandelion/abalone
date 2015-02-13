var React = require('react')
var Nav = require('./nav.jsx')

// var Navbar = require('react-bootstrap/Navbar')
// var Nav = require('react-bootstrap/Nav')
// var NavItem = require('react-bootstrap/NavItem')

module.exports = React.createClass({

  render: function() {
    return (
    <div>
      <div className="container">
        <nav className="navbar" role="navigation">

          <div className="navbar-header">
            <button type="button" className="navbar-toggle" data-toggle="collapse" data-target="#navbar-collapse">
              <span className="sr-only">Toggle navigation</span>
              <span className="icon-bar"></span>
              <span className="icon-bar"></span>
              <span className="icon-bar"></span>
            </button>
          </div>

          <div className="collapse navbar-collapse" id="navbar-collapse">
            <ul className="nav navbar-nav navbar-right">
              <li><a href="https://github.com/danmane/abalone" target="_blank"
                data-toggle="tooltip" data-placement="bottom"
                title="Github Repository"><i className="single fa fa-github"></i></a></li>
              <li><a href="https://github.com/danmane/abalone/issues/new" target="_blank"
                data-toggle="tooltip" data-placement="bottom"
                title="Report Bugs"><i className="single fa fa-bug"></i></a></li>
            </ul>
          </div>
        </nav>
      </div>

      {/* underline */}
      <div className="navhr" style={{margin: "10px 0px 30px"}}></div>

      <div className="container">
        {this.props.children}
      </div>
    </div>
    )
  }
})
