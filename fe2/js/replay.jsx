var React = require('react')

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
        <div class="replayChooser">
        <h1> Choose a Game </h1>
        <GameList data={this.state.data}/>
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
});

module.exports = ReplayChooser;
