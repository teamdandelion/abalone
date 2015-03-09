var React = require('react')
var Router = require('react-router')

var GameViewer = React.createClass({
  mixins: [Router.State],
  componentWillMount: function() {
    this.props.replayer = new Abalone.Frontend.GameReplayer();
  },
  componentDidMount: function() {
    var renderer = new Abalone.Frontend.Renderer("#replayerSVG");
    this.props.replayer.setRenderer(renderer);
    var url = "/api/games/" + this.getParams().gameId +"/states";
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
        console.error("error getting game at", urlistatus, err.toString())
      }.bind(this)
    });
  },
  render: function() {
    return (
      <div>
        <GameControls replayer={this.props.replayer}/>
        <svg id="replayerSVG" width="600" height="600"> </svg>
      </div>
    );
  }
})

var GameControls = React.createClass({
  render: function() {
    return (
        <div>
          <div className="row">
            <div className="col-md-2">
              <i onClick={this.props.replayer.play.bind(this.props.replayer)} className="fa fa-play fa-2x"></i>
            </div>
            <div className="col-md-2">
              <i onClick={this.props.replayer.pause.bind(this.props.replayer)} className="fa fa-pause fa-2x"></i>
            </div>
            <div className="col-md-2">
              <i onClick={this.props.replayer.back.bind(this.props.replayer)} className="fa fa-backward fa-2x"></i>
            </div>
            <div className="col-md-2">
              <i onClick={this.props.replayer.forward.bind(this.props.replayer)} className="fa fa-forward fa-2x"></i>
            </div>
            <div className="col-md-2">
              <i onClick={this.props.replayer.restart.bind(this.props.replayer)} className="fa fa-fast-backward fa-2x"></i>
            </div>
            <div className="col-md-2">
              <i onClick={this.props.replayer.skipToEnd.bind(this.props.replayer)} className="fa fa-fast-forward fa-2x"></i>
            </div>
          </div>
        </div>
      )
  }
})

module.exports = GameViewer
