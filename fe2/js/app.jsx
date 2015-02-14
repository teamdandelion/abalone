var $ = require('jquery')
var React = require('react')
var Router = require('react-router')
var RouteHandler = Router.RouteHandler
var Route = Router.Route
var DefaultRoute = Router.DefaultRoute
var NotFound = Router.NotFoundRoute
var BSNav = require('react-bootstrap/Nav')
var NavItem = require('react-bootstrap/NavItem')

var Input = require('react-bootstrap/Input');
var Button = require('react-bootstrap/Button');

var Nav = React.createClass({

  render: function() {
    return (
      <BSNav bsStyle="tabs" activeKey={this.props.activeKey}  style={{"marginBottom": "60px"}}>
        <NavItem eventKey={1} href="/"><i className="fa fa-trophy"></i> Leaderboard</NavItem>
        <NavItem eventKey={2} href="/play"><i className="fa fa-gamepad"></i> Play</NavItem>
        <NavItem eventKey={3} href="/upload"><i className="fa fa-upload"></i> Upload a Player</NavItem>
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
        <RouteHandler/>
      </div>
    </div>
    )
  }
})

var LeaderboardHandler = React.createClass({
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

var PlayHandler = React.createClass({
  render: function() {
    return (
      <div className="row">
        <div className="col-sm-8 col-sm-offset-2">

          <Nav activeKey={2} />

        </div>
      </div>
    )
  }
})

var UploadHandler = React.createClass({
  render: function() {
    var loading = false;
    if (loading) {
      view = (
        <Loading/>
      );
    } else {
      view = (
        <span>
          <div className="col-sm-6">
            <UploadPanel
              image="static/img/upload-logo-docker.png"
              message="Pull a Docker image from DockerHub"
              >
              <UploadForm btnMessage="Pull Image" url="api/v0/images" />
            </UploadPanel>
          </div>
          <div className="col-sm-6">
            <UploadPanel
              image="static/img/upload-logo-github.png"
              message="Build a Docker image from a GitHub Repo"
              >
              <UploadForm disabled="true" btnMessage="Build Image" url="api/v0/images" />
            </UploadPanel>
          </div>
        </span>
      );
    }
    return (
      <div className="row">
        <div className="col-sm-8 col-sm-offset-2">

          <Nav activeKey={3} />

          { view }

        </div>
      </div>
    )
  }
})

var UploadPanel = React.createClass({
  render: function() {
    return (
      <div>
        {this.props.message}
        <img className="upload-logo" src={this.props.image}></img>
        {this.props.children}
      </div>
    )
  }
})

var UploadForm = React.createClass({
  handleSubmit: function(e) {
    e.preventDefault();

    $.ajax({
      url: this.props.url,
      dataType: 'json',
      success: function(data) {
        this.setState({data: data});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error(status, err);
      }.bind(this)
    });
  },
  render: function() {
    return (
      <form className="uploadForm" onSubmit={this.handleSubmit}>
        <input
          type="text"
          ref="image"
          className="form-control"
          disabled={this.props.disabled}
        />
        <br/>
        <Button
          bsStyle="primary"
          bsSize="large"
          block
          disabled={this.props.disabled}>{this.props.btnMessage}</Button>
      </form>
    )
  }
})

var Loading = React.createClass({
  render: function() {
    return (
      <div className="row">
        loading...
      </div>
    )
  }
})

module.exports = (
  <Route name="app" path="/" handler={App}>
    <Route name="leaderboard" path="/leaderboard" handler={LeaderboardHandler} />
    <Route name="play" path="/play" handler={PlayHandler} />
    <Route name="upload" path="/upload" handler={UploadHandler} />
    <DefaultRoute handler={LeaderboardHandler} />
  </Route>
)
