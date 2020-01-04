puts "STARTED AT #{Time.now}"

NUM_USERS_FACTOR = 50
# MODELS = {
#   'help' =>  ['guan-actions-appended.csv', 100],
#   'custom-help' =>  ['custom-help.csv', 30],
#   'musegen' =>  ['actions-muse-gm-appended.csv', 30]
# }

# MODELS = {
#   'original' =>  ['gupended.csv', 100]
# }

MODELS = {
  'musegen-original4' =>  ['gupended.csv', 100]
}


def run_model(model_name, num_users, round)

  #puts model_name, scnariosUsersRatio

  startTime = Time.now
  puts "RUNNING test number #{round} with #{num_users} users. Model #{model_name}. Timestamp: #{startTime}"
  `ssh root@192.168.0.17 'sync; echo 3 > /proc/sys/vm/drop_caches'`
  sleep(5)
  `ssh vinicius@192.168.0.17 'echo kHik5c#\{z/Gu | sudo -S rm /var/log/nginx/access.log'`
  `ssh vinicius@192.168.0.17 'mkdir -p /home/vinicius/tileserver-gl-logs/#{model_name}'`
  
  Thread.new {
    `ssh vinicius@192.168.0.17 '/home/vinicius/.nvm/versions/node/v6.11.3/bin/tileserver-gl ~/tileserver/osm-2019-09-30-v3.10-planet.mbtiles -p 8081 > /dev/null 2>&1 & echo $! > run1.pid'`
    `ssh vinicius@192.168.0.17 '/home/vinicius/.nvm/versions/node/v6.11.3/bin/tileserver-gl ~/tileserver/osm-2019-09-30-v3.10-planet.mbtiles -p 8082 > /dev/null 2>&1 & echo $! > run2.pid'`
    `ssh vinicius@192.168.0.17 '/home/vinicius/.nvm/versions/node/v6.11.3/bin/tileserver-gl ~/tileserver/osm-2019-09-30-v3.10-planet.mbtiles -p 8083 > /dev/null 2>&1 & echo $! > run3.pid'`
    `ssh vinicius@192.168.0.17 '/home/vinicius/.nvm/versions/node/v6.11.3/bin/tileserver-gl ~/tileserver/osm-2019-09-30-v3.10-planet.mbtiles -p 8084 > /dev/null 2>&1 & echo $! > run4.pid'`
    `ssh vinicius@192.168.0.17 '/home/vinicius/.nvm/versions/node/v6.11.3/bin/tileserver-gl ~/tileserver/osm-2019-09-30-v3.10-planet.mbtiles -p 8085 > /dev/null 2>&1 & echo $! > run5.pid'`
    `ssh vinicius@192.168.0.17 '/home/vinicius/.nvm/versions/node/v6.11.3/bin/tileserver-gl ~/tileserver/osm-2019-09-30-v3.10-planet.mbtiles -p 8086 > /dev/null 2>&1 & echo $! > run6.pid'`
    `ssh vinicius@192.168.0.17 '/home/vinicius/.nvm/versions/node/v6.11.3/bin/tileserver-gl ~/tileserver/osm-2019-09-30-v3.10-planet.mbtiles -p 8087 > /dev/null 2>&1 & echo $! > run7.pid'`
    `ssh vinicius@192.168.0.17 '/home/vinicius/.nvm/versions/node/v6.11.3/bin/tileserver-gl ~/tileserver/osm-2019-09-30-v3.10-planet.mbtiles -p 8088 > /dev/null 2>&1 & echo $! > run8.pid'`
    #`ssh vinicius@192.168.0.17 '/home/vinicius/.nvm/versions/node/v6.11.3/bin/tileserver-gl ~/tileserver/osm-2019-09-30-v3.10-planet.mbtiles -p 8085 > /home/vinicius/tileserver-gl-logs/#{j*NUM_USERS_FACTOR}-users-round-#{i}-server-5.log 2>&1 & echo $! > run5.pid'`
    #`ssh vinicius@192.168.0.17 '/home/vinicius/.nvm/versions/node/v6.11.3/bin/tileserver-gl ~/tileserver/osm-2019-09-30-v3.10-planet.mbtiles -p 8086 > /home/vinicius/tileserver-gl-logs/#{j*NUM_USERS_FACTOR}-users-round-#{i}-server-6.log 2>&1 & echo $! > run6.pid'`
  }
  
  sleep(5)
  `ssh vinicius@192.168.0.17 'echo kHik5c#\{z/Gu | sudo -S systemctl restart nginx'`
  `mkdir -p ./results/#{model_name}/#{num_users}-users/round-#{round}`

  `env SESSION_FILE='#{model_name}_urls_in_sessions.txt' locust -f load_test/locust_files/wms_user.py --slave > /dev/null 2>&1 &`
  `env SESSION_FILE='#{model_name}_urls_in_sessions.txt' locust -f load_test/locust_files/wms_user.py --slave > /dev/null 2>&1 &`
  `env SESSION_FILE='#{model_name}_urls_in_sessions.txt' locust -f load_test/locust_files/wms_user.py --slave > /dev/null 2>&1 &`
  `env SESSION_FILE='#{model_name}_urls_in_sessions.txt' locust -f load_test/locust_files/wms_user.py --slave > /dev/null 2>&1 &`
  start_test_time = Time.now.to_i
  end_test_time = start_test_time + 610
  `env SESSION_FILE='#{model_name}_urls_in_sessions.txt' locust -f load_test/locust_files/wms_user.py --no-web -c #{num_users} -r #{num_users} --only-summary --csv=./results/#{model_name}/#{num_users}-users/round-#{round}/output --run-time 10m --master > /dev/null 2>&1`

  metrics_dir = "./results/#{model_name}/#{num_users}-users/round-#{round}/metrics"
  `mkdir -p #{metrics_dir}`

  
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=apps.vmem&dimension=node&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/vmem_history.json` 
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=apps.mem&dimension=node&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/mem_history.json`    
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=system.ram&dimension=used&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/ram_history.json`
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=apps.cpu_user&dimension=node&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/app_cpu_user_history.json`    
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=apps.cpu_system&dimension=node&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/app_cpu_system_history.json`    
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=apps.cpu&dimension=node&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/app_cpu_history.json` 
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=system.cpu&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/cpu_history.json` 
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=system.cpu&dimension=user&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/cpu_user_history.json` 
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=system.cpu&dimension=system&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/cpu_system_history.json` 
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=system.cpu&dimension=softirq&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/cpu_softirq_history.json` 
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=apps.sockets&dimension=node&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/sockets_history.json`    
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=apps.threads&dimension=node&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/threads_history.json`   
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=apps.pwrites&dimension=node&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/dwrites_history.json`    
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=apps.preads&dimension=node&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/dreads_history.json`    
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=net_packets.enp7s0&dimensions=received&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/enp_pack_received_history.json`    
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=net_packets.enp7s0&dimensions=sent&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/enp_pack_sent_history.json`    
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=net.enp7s0&format=json&points=630&group=average&gtime=0&options=ms%7Cflip%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/enp_pack_rec_sent_history.json`
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=system.net&dimension=received&format=json&points=630&group=average&gtime=0&options=absolute%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/net_received_history.json`    
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=system.net&dimension=sent&format=json&points=630&group=average&gtime=0&options=absolute%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/net_sent_history.json`    
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=net.enp7s0&dimension=received&format=json&points=630&group=average&gtime=0&options=absolute%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/net_enp_received_history.json`    
  `curl 'http://192.168.0.17:19999/api/v1/data?chart=net.enp7s0&dimension=sent&format=json&points=630&group=average&gtime=0&options=absolute%7Cjsonwrap%7Cnonzero&after=#{start_test_time}&before=#{end_test_time}' > #{metrics_dir}/net_enp_sent_history.json`    
  `curl 'http://192.168.0.17:80/nginx_status' > #{metrics_dir}/nginx_status.txt`
  `ssh vinicius@192.168.0.17 'kill -9 $(cat ~/run1.pid)'`
  `ssh vinicius@192.168.0.17 'kill -9 $(cat ~/run2.pid)'`
  `ssh vinicius@192.168.0.17 'kill -9 $(cat ~/run3.pid)'`
  `ssh vinicius@192.168.0.17 'kill -9 $(cat ~/run4.pid)'`
  `ssh vinicius@192.168.0.17 'kill -9 $(cat ~/run5.pid)'`
  `ssh vinicius@192.168.0.17 'kill -9 $(cat ~/run6.pid)'`
  `ssh vinicius@192.168.0.17 'kill -9 $(cat ~/run7.pid)'`
  `ssh vinicius@192.168.0.17 'kill -9 $(cat ~/run8.pid)'`
  #{}`ssh vinicius@192.168.0.17 'docker stop omt2'`

  `ssh vinicius@192.168.0.17 'cp /var/log/nginx/access.log /home/vinicius/tileserver-gl-logs/#{model_name}/#{num_users}-access-round-#{round}.log'`

  # copy results to HD
  `mkdir -p /media/arquivos/testes/#{model_name}/#{num_users}-users/`
  `mv ./results/#{model_name}/#{num_users}-users/round-#{round}/ /media/arquivos/testes/#{model_name}/#{num_users}-users/`

  endTime = Time.now
  puts "Test duration: #{endTime - startTime} seconds"
  sleep(10)
end

for round in 10..30
  startJ = 1

  # if round == 6
  #   startJ = 6
  # end
  # if round == 15 or round == 16 or round == 17 or round == 18 or round == 19
  #   next
  # end

  for j in startJ..7
    for m, f in MODELS
      # if round == 25 and ['help', 'custom-help'].include? m
      #   puts "JUMP MODEL #{m}"
      #   next
      # end
      num_users = j*NUM_USERS_FACTOR
      run_model(m, num_users, round)
    end
  end
end