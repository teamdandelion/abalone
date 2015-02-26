var React = require('react')
var Router = require('react-router')
var RouteHandler = Router.RouteHandler
var Route = Router.Route
var Link = Router.Link
var DefaultRoute = Router.DefaultRoute
var NotFound = Router.NotFoundRoute
var BSNav = require('react-bootstrap/Nav')
var NavItemLink = require('react-router-bootstrap').NavItemLink

var Input = require('react-bootstrap/Input');

var UploadHandler = require('./upload.jsx')
var LeaderboardHandler = require('./leaderboard.jsx')
var PlayHandler = require('./play.jsx')
var ReplayChooser = require('./replay.jsx')
var GameViewer = require('./viewGame.jsx')

var Nav = React.createClass({
  render: function() {
    return (
      <BSNav bsStyle="tabs" activeKey={this.props.activeKey}  style={{"marginBottom": "60px"}}>
        <NavItemLink to="leaderboard"><i className="fa fa-trophy"></i> Leaderboard</NavItemLink>
        <NavItemLink to="play"><i className="fa fa-gamepad"></i> Play</NavItemLink>
        <NavItemLink to="upload"><i className="fa fa-upload"></i> Upload a Player</NavItemLink>
        <NavItemLink to="replay"><i className="fa fa-repeat"></i> Replay a Game</NavItemLink>
      </BSNav>
    )
  }
})

var App = React.createClass({
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
          <div className="row">
            <div className="col-sm-8 col-sm-offset-2">
            <Nav />
              <RouteHandler/>
            </div>
          </div>
        </div>
      </div>
    )
  }
})

module.exports = (
  <Route name="app" path="/" handler={App}>
    <Route name="leaderboard" path="/leaderboard" handler={LeaderboardHandler} />
    <Route name="play" path="/play" handler={PlayHandler} />
    <Route name="upload" path="/upload" handler={UploadHandler} />
    <Route name="replay" path="/replay" handler={ReplayChooser} />
    <Route name="viewGame" path="/viewGame/:gameId" handler={GameViewer} />
    <DefaultRoute name="leaderboardDefault" handler={LeaderboardHandler} />
  </Route>
)
