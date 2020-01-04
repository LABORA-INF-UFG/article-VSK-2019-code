import load_test.util.constants as constants
import linecache


class Action:

    def __init__(self, tiles, interval):
        self.tiles = tiles
        self.interval = interval


class Session:

    def __init__(self, id, actions_list):
        self.id = id
        self.actions_list = actions_list


class SessionUtil:

    @staticmethod
    def read_sessions():
        filename = constants.SESSION_FILE
        sessions = []

        with open(filename, 'r') as f:
            num_lines = sum(1 for line in f)

            print("num_lines: {}".format(num_lines))

            for i in range(0, num_lines):
                session = SessionUtil.parse_session_line(filename, i)

                sessions.append(session)

        return sessions

    @staticmethod
    def parse_session_line(filename, linenumber):
        line = linecache.getline(filename, linenumber)
        actions_str_list = line.split(";")
        actions_list = []

        for action_str in actions_str_list[0:-1]:
            action_str = action_str.replace("[", "").replace("]", "")
            action_part = action_str.split(",")

            if "/" not in action_str:
                interval = int(action_part[-1])
                action = Action(None, interval)
                actions_list.append(action)
                continue

            interval = int(action_part[-1])
            tiles = []

            for ad in action_part[0:-1]:
                tiles.append(ad.strip())

            action = Action(tiles, interval)
            actions_list.append(action)

        session = Session(linenumber, actions_list)
        return session
