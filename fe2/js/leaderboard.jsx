var React = require('react')
var Table = require('react-bootstrap').Table

var LeaderboardHandler = React.createClass({
  getInitialState: function() {
    return {rankings: []};
  },

  loadRankingsFromServer: function() {
    $.ajax({
      url: '/api/rankings',
      dataType: 'json',
      success: function(data) {
        console.log("data");
        this.setState({rankings: data});
        console.log("state set");
      }.bind(this),
      error: function(xhr, status, err) {
        console.error("erorr getting api/rankings", status, err.toString());
      }.bind(this)
    })
  },

  componentWillMount: function() {
    console.log("will mount");
    this.loadRankingsFromServer();
  },

  render: function() {
    console.log("rendered");
    console.log(this.state.rankings);
    return (
      <RankingsTable rankings={this.state.rankings}>
      </RankingsTable>
    )
  }
})

var RankingsTable = React.createClass({
  render: function() {
    console.log(this.props.rankings);
    var rows = this.props.rankings.map(function (r) {
      return (
        <tr>
          <td>{r.rank}</td>
          <td>{r.player}</td>
          <td>{r.author}</td>
          <td>{r.rating}</td>
          <td className="text-center">{r.wins}</td>
          <td className="text-center">{r.losses}</td>
        </tr>
      );
    });
    return (
      <Table>
        <tr>
          <th>#</th>
          <th>Player</th>
          <th>Author</th>
          <th>Rating</th>
          <th className="text-center">W</th>
          <th className="text-center">L</th>
        </tr>
        {rows}
      </Table>
    )
  }
})

module.exports = LeaderboardHandler;
