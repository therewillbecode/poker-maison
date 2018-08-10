/**
 *
 * App
 *
 * This component is the skeleton around the actual pages, and should only
 * contain code that should be seen on all pages. (e.g. navigation bar)
 */

import React from 'react';
import { Helmet } from 'react-helmet';
import { Switch, Route } from 'react-router-dom';

import HomeContainer from '../containers/HomeContainer';
import NavBarContainer from '../containers/NavBarContainer';
import Signout from './Signout'
import SignupFormContainer from '../containers/SignupFormContainer'
import SignInFormContainer from '../containers/SignInFormContainer'
import LobbyContainer from '../containers/LobbyContainer'

import Footer from './Footer';
import NotFoundPage from './NotFoundPage';

const App = () => (
    <div className="app-wrapper">
        <Helmet
            titleTemplate="%s - React.js Boilerplate"
            defaultTitle="React.js Boilerplate"
        >
            <meta name="description" content="A React.js Boilerplate application" />
        </Helmet>
        <NavBarContainer />
        <Switch>
            <Route exact path="/" component={HomeContainer} />
            <Route path="/signup" component={SignupFormContainer} />
            <Route path="/signin" component={SignInFormContainer} />
            <Route path="/signout" component={Signout} />
            <Route path="/lobby" component={LobbyContainer} />
            <Route path="" component={NotFoundPage} />
        </Switch>
        <Footer />
    </div>
);

export default App;
