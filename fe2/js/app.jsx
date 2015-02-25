var superagent = require('superagent')
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
var Button = require('react-bootstrap/Button');

var Flux = require('flux')
var Dispatcher = Flux.Dispatcher;
var assign = require('object-assign');
var keyMirror = require('keymirror');
var EventEmitter = require('events').EventEmitter;

var Nav = React.createClass({

  render: function() {
    return (
      <BSNav bsStyle="tabs" activeKey={this.props.activeKey}  style={{"marginBottom": "60px"}}>
        <NavItemLink eventKey={1} to="leaderboard"><i className="fa fa-trophy"></i> Leaderboard</NavItemLink>
        <NavItemLink eventKey={2} to="play"><i className="fa fa-gamepad"></i> Play</NavItemLink>
        <NavItemLink eventKey={3} to="upload"><i className="fa fa-upload"></i> Upload a Player</NavItemLink>
        <NavItemLink eventKey={4} to="replay"><i className="fa fa-repeat"></i> Replay a Game</NavItemLink>
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

var ReplayChooser = React.createClass({
  getInitialState: function() {
    return {data: []};
  },
  loadGamesFromServer: function() {
    $.ajax({
      url: '/api/games',
      dataType: 'json',
      success: function(data) {
        this.setState({data: data});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error("erorr getting api/games", status, err.toString());
      }.bind(this)
    })
  },
  componentDidMount: function() {
    this.loadGamesFromServer()
  },
  render: function() {
    return (
      <div className="row">
        <div className="col-sm-8 col-sm-offset-2">

          <Nav activeKey={2} />

          <div class="replayChooser">
          <h1> Choose a Game </h1>
          <GameList data={this.state.data}/>
          </div>
        </div>
      </div>
      );
  }
});

var GameList = React.createClass({
  render: function() {
    var gameNodes = this.props.data.map(function (game) {
      return (
        <GameRow data={game}> </GameRow>
      );
    });
    return (
      <div className="gameList">
        {gameNodes}
      </div>
    );
  }
});

var GameRow = React.createClass({
  render: function() {
    var dest = "/viewGame/" + this.props.data.ID;
    return (
      <div className="gameRow"> <a href={dest}>{this.props.data.ID} {this.props.data.Status}</a> </div>
      )
  }
})

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

var GameViewer = React.createClass({
  mixins: [Router.State],
  render: function() {
    return (
      <div className="row">
        <div className="col-sm-8 col-sm-offset-2">
          <Nav activeKey={4} />
          <GameReplayer id={this.getParams().gameId}> </GameReplayer>
        </div>
      </div>
    );
  }
})

var GameReplayer = React.createClass({
  getInitialState: function() {
    return {data: []};
  },
  // loadHistoryFromServer: function() {
  //   var url = 'api/games/'+this.props.gameID+'/states';
  //   $.ajax({
  //     url: url,
  //     dataType: 'json',
  //     success: function(data) {
  //       this.setState({data: data});
  //     }.bind(this),
  //     error: function(xhr, status, err) {
  //       console.error("error getting game states at", url, status, err.toString())
  //     }.bind(this)
  //   })
  // },
  componentDidMount: function() {
    var renderer = new Abalone.Frontend.Renderer("#replayerSVG");
    // loadHistoryFromServer();
    this.props.replayer = new Abalone.Frontend.GameReplayer(renderer, []);
    var url = "/api/games/" + this.props.id +"/states"
    $.ajax({
      url: url,
      dataType: 'json',
      success: function(data) {
        data.forEach(Abalone.Engine.parseJSON)
        this.props.replayer.setHistory(data);
        this.props.replayer.play();
      }.bind(this),
      error: function(xhr, status, err) {
        console.log(xhr.responseText);
        console.error("error getting game at", url, status, err.toString())
      }.bind(this)
    });
  },
  render: function() {
    return (
      <svg id="replayerSVG" width="800" height="800"> </svg>
    );
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

var UIDispatcher = assign(new Dispatcher(), {
  handleAction: function(action) {
    var payload = {
      action: action
    };
    this.dispatch(payload);
  }
});

var UIConstants = module.exports = {
  ActionTypes: keyMirror({
    IMAGES_UPLOAD_SUBMIT_FORM: null,
  }),
};
var ActionTypes = UIConstants.ActionTypes;

var ImagesStore = assign({loading: false}, EventEmitter.prototype, {
  emitChange: function() {
    this.emit(CHANGE_EVENT);
  },
  addChangeListener: function(callback) {
    this.on(CHANGE_EVENT, callback);
  },
  removeChangeListener: function(callback) {
    this.removeListener(CHANGE_EVENT, callback);
  },
  getState: function() {
    return {
      loading: this.loading
    }
  },
});
var CHANGE_EVENT = 'change';

var AuthStore = assign({
  emailAddress: "",
  token: "",
  isLoggedIn: false
}, EventEmitter.prototype, {
  emitChange: function() {
    this.emit(CHANGE_EVENT);
  },
  addChangeListener: function(callback) {
    this.on(CHANGE_EVENT, callback);
  },
  removeChangeListener: function(callback) {
    this.removeListener(CHANGE_EVENT, callback);
  },
  getState: function() {
    return {
      emailAddress: this.emailAddress,
      token: this.token,
      isLoggedIn: this.isLoggedIn
    }
  },
});

AuthStore.dispatchToken = UIDispatcher.register(function(payload) {
  var action = payload.action;
  switch(action.type) {
    default:
  }
});


ImagesStore.dispatchToken = UIDispatcher.register(function(payload) {
  var action = payload.action;

  switch(action.type) {
    case ActionTypes.IMAGES_UPLOAD_SUBMIT_FORM:
      ImagesStore.loading = true;
      superagent
        .post(action.data.url)
        .send({
          source: action.data.source,
          image: action.data.image
        })
        .set('Accept', 'application/json')
        .end(function(err, res) {
          console.log(res.status);
          ImagesStore.loading = false;
          ImagesStore.emitChange();
        });
      ImagesStore.emitChange();
      break;

    default:
  }
});

module.exports = (
  <Route name="app" path="/" handler={App}>
    <Route name="leaderboard" path="/leaderboard" handler={LeaderboardHandler} />
    <Route name="play" path="/play" handler={PlayHandler} />
    <Route name="upload" path="/upload" handler={UploadHandler} />
    <Route name="replay" path="/replay" handler={ReplayChooser} />
    <Route name="viewGame" path="/viewGame/:gameId" handler={GameViewer} />
    <DefaultRoute handler={LeaderboardHandler} />
  </Route>
)
