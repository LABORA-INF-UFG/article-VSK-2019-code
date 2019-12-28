from erss.model.session import Session
from erss.model.url import URL
from erss.util.url_util import URLUtil
from erss.util.tile_util import TileUtil
import copy


class ModelUtil:

    @staticmethod
    def extract_sessions(json_content):
        session_list = []
        for s in json_content:
            user = s['User']
            date = s['Date']
            urls = s['Urls']
            app_id = s['AppId']
            resolution = s['Resolution']

            if 'usernum' not in user:
                continue

            last_url_obj = URL.url_dict_to_obj(urls[0])
            steps = [[last_url_obj.coordinate, 0]]
            if last_url_obj.resolution == '':
                last_url_obj.resolution = resolution
            tiles_list = TileUtil.list_tiles(last_url_obj.coordinate, last_url_obj.resolution)
            steps_tiles = [[tiles_list, 0]]
            if tiles_list is None:
                steps_tiles = [[0, 0]]
            urls_obj_list = []

            for url in urls[1:]:
                url_obj = URL.url_dict_to_obj(url)

                think_time = int(url_obj.timestamp - last_url_obj.timestamp)

                steps[-1][1] = think_time
                steps_tiles[-1][1] = think_time

                if steps_tiles[-1][0] == 0:
                    steps_tiles[-1][0] = think_time

                steps.append([url_obj.coordinate, 0])

                # if not (URLUtil.check_if_is_satellite_map(last_url_obj.url) or URLUtil.check_if_is_street_view(
                #         last_url_obj.url) or URLUtil.check_if_is_satellite_map(
                #     url_obj.url) or URLUtil.check_if_is_street_view(url_obj.url)):

                if url_obj.resolution == '':
                    url_obj.resolution = resolution

                tiles_list = TileUtil.list_tiles(url_obj.coordinate, url_obj.resolution)

                if tiles_list is not None:
                    steps_tiles.append([tiles_list, 0])
                else:
                    steps_tiles.append([0, 0])


                last_url_obj = url_obj
                urls_obj_list.append(last_url_obj)

            session = Session(user, date, urls_obj_list, steps, steps_tiles, app_id)

            sessions = ModelUtil.divide_session_by_tab_id(session)

            session_list = session_list + sessions

        filter_sessions = ModelUtil.filter_sessions(session_list)

        return filter_sessions

    @staticmethod
    def divide_session_by_tab_id(session):
        sessions = []
        tab_ids = []
        urls_by_tab = {}
        for url in session.urls:
            if url.tabId not in urls_by_tab:
                urls_by_tab[url.tabId] = [url]
            else:
                urls_by_tab[url.tabId].append(url)

        if len(urls_by_tab) == 1:
            sessions.append(session)
        else:
            for k,urls in urls_by_tab.items():
                session_copy = copy.deepcopy(session)
                session_copy.urls = urls
                sessions.append(session_copy)

        return sessions

    @staticmethod
    def filter_sessions(sessions):
        filtered_sessions = []
        for s in sessions:
            is_just_street_map = True
            for u in s.urls:
                if URLUtil.check_if_is_satellite_map(u.url):
                    is_just_street_map = False
                if URLUtil.check_if_is_street_view(u.url):
                    is_just_street_map = False

            if is_just_street_map:
                filtered_sessions.append(s)

        return filtered_sessions


    @staticmethod
    def check_if_session_has_tiles(session):
        tiles_list = session.steps_tiles
        has_tiles = False
        for t in tiles_list:
            if type(t[0]) is list:
                has_tiles = True
        return has_tiles

