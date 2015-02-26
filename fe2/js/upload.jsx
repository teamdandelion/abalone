var loading = require('./loading.jsx')
var React = require('react')
var UIDispatcher = require('./flux.jsx').UIDispatcher
var ImagesStore = require('./flux.jsx').ImagesStore
var Button = require('react-bootstrap/Button');


var UploadHandler = React.createClass({
  getInitialState: function() {
    return {
      loading: false
    }
  },
  componentDidMount: function() {
    ImagesStore.addChangeListener(this._onChange);
  },
  componentWillUnmount: function() {
    ImagesStore.removeChangeListener(this._onChange);
  },
  _onChange: function() {
    this.setState(ImagesStore.getState());
  },
  render: function() {
    if (this.state.loading) {
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
              <UploadForm source="dockerhub" btnMessage="Pull Image" url="api/v0/images" />
            </UploadPanel>
          </div>
          <div className="col-sm-6">
            <UploadPanel
              image="static/img/upload-logo-github.png"
              message="Build a Docker image from a GitHub Repo"
              >
              <UploadForm source="github" disabled={true} btnMessage="Build Image" url="api/v0/images" />
            </UploadPanel>
          </div>
        </span>
      );
    }
    return (
      <div>
      { view }
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
});

var UploadForm = React.createClass({
  handleSubmit: function(e) {
    UIDispatcher.handleAction({
      type: ActionTypes.IMAGES_UPLOAD_SUBMIT_FORM,
      data: {
        source: this.props.source,
        image: this.refs.image.getDOMNode().value.trim(),
        url: this.props.url
      }
    });
    e.preventDefault();
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

module.exports = UploadHandler;

