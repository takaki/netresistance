import React, {Component} from "react";
import logo from "./logo.svg";
import "./App.css";
import MuiThemeProvider from "material-ui/styles/MuiThemeProvider";
import {
    Table,
    TableBody,
    TableHeader,
    TableHeaderColumn,
    TableRow,
    TableRowColumn
} from "material-ui/Table";
import TextField from "material-ui/TextField";
import {List, ListItem} from "material-ui/List";


let data = {
    "round": {
        "vote": [{
            "user0": "A",
            "user1": "R",
            "user2": "A",
            "user3": "A",
            "user4": "A"
        }],
        "mission": {}
    }
};

class App extends Component {

    render() {
        return (
            <MuiThemeProvider>
                <div>
                    <div className="App">
                        <div className="App-header">
                            <img src={logo} className="App-logo" alt="logo"/>
                            <h2>Welcome to React</h2>
                        </div>
                        <p className="App-intro">
                            To get started, edit <code>src/App.js</code> and
                            save to
                            reload.
                        </p>
                    </div>
                    <MemberList/>
                    <TrackBoard/>
                    <ChatBox/>
                    <ChatInput/>
                </div>
            </MuiThemeProvider>
        );
    }

}


class TrackBoard extends Component {
    render() {
        return (
            <Table>
                <TableHeader>
                    <TableRow>
                        <TableHeaderColumn>
                            Round1
                        </TableHeaderColumn>
                    </TableRow>
                    <TableRow>
                        <TableRowColumn>
                            123
                        </TableRowColumn>
                    </TableRow>
                </TableHeader>
            </Table>
        )
    }
}

class MemberList extends Component {
    render () {
        return (
            <List>
                <ListItem primaryText="member0"/>
                <ListItem primaryText="member1"/>
                <ListItem primaryText="member2"/>
                <ListItem primaryText="member3"/>
                <ListItem primaryText="member4"/>

            </List>
        )
    }
}

class ChatBox extends Component {
    render() {
        return (
            <div>
                <div>member1&gt; hoge foo bar</div>
            </div>
        )

    }
}


class ChatInput extends Component {
    render() {
        return (
            <TextField fullWidth={true}
                       onKeyDown={(ev) => {
                           console.log(ev.keyCode === 13);
                           console.log(ev.target.value);
                       }}/>
        )
    }

}

export default App;
