var React = require('react')
var Router = require('react-router')

var GameViewer = React.createClass({
  mixins: [Router.State],
  componentDidMount: function() {
    var renderer = new Abalone.Frontend.Renderer("#replayerSVG");
    this.props.replayer = new Abalone.Frontend.GameReplayer(renderer, []);
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
      <svg id="replayerSVG" width="800" height="800"> </svg>
    );
  }
})

module.exports = GameViewer
