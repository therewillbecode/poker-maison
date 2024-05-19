import axios from 'axios'

import * as types from './types'

/* Action Creators for User API authentication */
const AUTH_API_URL = 'http://localhost:3000'
//process.env.NODE_ENV === 'production' ? 'https://tenpoker.co.uk' : 'http://localhost:8000'

export const getProfileRequest = () => ({ type: types.GET_PROFILE_REQUEST })

export const getProfileSuccess = profile => ({
  type: types.GET_PROFILE_SUCCESS,
  profile
})

export const getProfileErr = error => ({ type: types.GET_PROFILE_ERR, error })

export const getProfile = username => {
  return dispatch => {
    const token = localStorage.getItem('token')
    if (token) {
      try {
        const { access_token } = JSON.parse(token)
        dispatch(getProfileRequest())
        console.log('access token', access_token)
        axios
          .get(`${AUTH_API_URL}/profile`, {
            headers: {
              Authorization: access_token,
              'Content-Type': 'application/json'
            }
          })
          .then(({ data }) => {
            const profile = {
              chipsInPlay: data.proChipsInPlay,
              availableChips: data.proAvailableChips,
              userCreatedAt: data.proUserCreatedAt,
              username: data.proUsername,
              email: data.proEmail
            }

            dispatch(getProfileSuccess(profile))
          })
          .catch(err => dispatch(getProfileErr(err)))
      } catch (e) {
        console.log(e)
        dispatch(getProfileErr(e))
      }
    } else {
      dispatch(getProfileErr('No JWT token for profile request'))
    }
  }
}
