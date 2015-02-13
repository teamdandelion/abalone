var $ = require('jquery')
var React = require('react')
var Router = require('react-router')
var Routes = Router.Routes
var Route = Router.Route
var NotFound = Router.NotFoundRoute
var Page = require('./views/page.jsx')
var Nav = require('./views/nav.jsx')
var NotFoundPage = require('./pages/notfound.jsx')

var Input = require('react-bootstrap/Input');
var Button = require('react-bootstrap/Button');


module.exports = React.createClass({
  render: function() {

    return (
      <Page>
        <Routes location="history">
          <Route name="home" path="/" handler={LeaderboardHandler} />
          <Route name="play" path="/play" handler={PlayHandler} />
          <Route name="upload" path="/upload" handler={UploadHandler} />
          <NotFound handler={NotFoundPage} />
        </Routes>
      </Page>
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
    return (
      <div className="row">
        <div className="col-sm-8 col-sm-offset-2">

          <Nav activeKey={3} />

          <div className="col-sm-6">
            <DockerHubUploadPanel />
          </div>
          <div className="col-sm-6">
            <GitHubUploadPanel />
          </div>

        </div>
      </div>
    )
  }
})

var DockerHubUploadPanel = React.createClass({
  render: function() {
    return (
      <div>
        Pull a Docker image from DockerHub 
        <img className="upload-logo" src="static/img/upload-logo-docker.png"></img>
        <DockerHubUploadForm url="api/v0/image" />
      </div>
    )
  }
})

var DockerHubUploadForm = React.createClass({
  handleSubmit: function(e) {
    e.preventDefault();
    var image = this.refs.image.getDOMNode().value;
    this.refs.image.getDOMNode().value = "";
    if (!image) {
      return;
    }
  },
  render: function() {
    return (
      <form className="uploadForm" onSubmit={this.handleSubmit}>
        <input type="text" ref="image" className="form-control" />
        <br/>
        <Button bsStyle="primary" bsSize="large" block>Pull Image</Button>
      </form>
    )
  }
})

var GitHubUploadPanel = React.createClass({
  render: function() {
    return (
      <div>
        Build a Docker image from a GitHub Repo
        <img className="upload-logo" src="static/img/upload-logo-github.png"></img>
        <GitHubUploadForm />
      </div>
    )
  }
})

var GitHubUploadForm = React.createClass({
  handleSubmit: function(e) {
    e.preventDefault();
    console.log(this.refs.repo.getDOMNode().value);
    var repo = this.refs.repo.getDOMNode().value.trim();
    if (!repo) {
      return;
    }
    this.refs.repo.getDOMNode().value = '';
  },
  render: function() {
    return (
      <form className="uploadForm" onSubmit={this.handleSubmit}>
        <input type="text" ref="repo" className="form-control" disabled/>
        <br/>
        <Button bsStyle="primary" bsSize="large" block disabled>Build Image</Button>
      </form>
    )
  }
})
