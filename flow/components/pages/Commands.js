// @flow strict

import type { ContextRouter } from 'react-router-dom';
import * as React from 'react';

import API from 'codegen/API';

type Props = {
  ...ContextRouter,
};

type State = { commands: Array<string> };

class Commands extends React.Component<Props, State> {
  state = { commands: [] };

  async componentDidMount() {
    const response = await API.getCommands(document.cookie);
    this.setState({ commands: response });
  }

  render() {
    return <div>{JSON.stringify(this.state.commands)}</div>;
  }
}

export default Commands;
