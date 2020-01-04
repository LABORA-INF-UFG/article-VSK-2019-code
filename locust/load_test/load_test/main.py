from load_test.util.session_util import SessionUtil
import pickle
from os import path

sessions = None

if path.exists("sessions.pickle"):
    with open('sessions.pickle', 'rb') as sessions_pickle:
        sessions = pickle.load(sessions_pickle)

if not sessions:
    sessions = SessionUtil.read_sessions()

if not path.exists("sessions.pickle"):
    with open('sessions.pickle', 'wb') as sessions_pickle:
        pickle.dump(sessions, sessions_pickle)

print(len(sessions))

for s in sessions[0:10]:
    print(len(s.actions_list))
    for a in s.actions_list:
        print(a.__dict__)