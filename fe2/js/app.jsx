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
    var image = this.refs.image.getDOMNode().value;
    this.refs.image.getDOMNode().value = "";
    if (!image) {
      return;
    }
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

