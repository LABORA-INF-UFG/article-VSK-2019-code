from erss.util.file_util import FileUtil
from erss.util.model_util import ModelUtil
import erss.util.constants as constants

file_util = FileUtil()

sessions_file = file_util.read_json_file(constants.DATA_PATH)

sessions = ModelUtil.extract_sessions(sessions_file)

# with open('musegen_tiles.csv', 'w+') as file:
#     for s in sessions:
#         if ModelUtil.check_if_session_has_tiles(s):
#             file.write("{}\n".format(";".join([str(x) for x in s.steps_tiles])))
#         else:
#             print(s.steps_tiles)


with open('coordinates.csv', 'w+') as file:
    for s in sessions:
        if ModelUtil.check_if_session_has_tiles(s):
            for step in s.steps:
                if None not in step[0]:
                    file.write("{}\n".format(";".join([str(x) for x in step[0]])))
                    break
        else:
            print(s.steps)

