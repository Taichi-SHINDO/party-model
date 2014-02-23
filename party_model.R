num_person <- 1:10	# 人数 (n対nの合コン)	
goukon_num <- 1:10000	# 調べる回数

my_charm <- 0.5 	# 僕の魅力度
my_lower_bound <- 0.4	# 僕の許容範囲の下限

success_num <- c()	# n対nの合コンで付き合えた回数



# 合コンの場を仮想的に作る(男性陣)
init_goukon_state_man <- function(man, num) {

	# 男性陣の魅力度を決定
	for (i in 1:num) {
		if (i != 1) {
			man_power <- 0.5 + (rnorm(1) / 10) 
			man <- c(man, man_power)
		}
	}
	return(man)
}

# 合コンの場を仮想的に作る(女性陣)
init_goukon_state_woman <- function(woman, num) {

	# 女性陣の魅力度を決定
	for (i in 1:num) {
		woman_power <- 0.5 + (rnorm(1) / 10)
		woman <- c(woman, woman_power)
	}
	return(woman)
}

# アプローチする相手を選択
to_approach <- function(man, woman, match, my_lower_bound) {
	man_id <- 1		
	for (i in man) {
		min <- 1;
		pair <- -1;
		woman_id <- 1;
		for (j in woman) {
			if ((man_id == 1) && (woman[woman_id] < my_lower_bound)) {
			}
			else if (abs(i - j) < min) {
				min = abs(i - j)
				pair <- woman_id;
			}	
			woman_id <- woman_id + 1
		}
		match = c(match, pair)
		man_id <- man_id + 1
	}
	return(match)
}

# 一人の女性を奪いあう状況のとき
find_rival <- function(man, woman, match) {
	man_id <- 1
	for (i in match) {
		rival_id <- 1
		for (j in match) {
			if ((man_id != rival_id) && (i == j)) {
				if (man[man_id] < man[rival_id]) {
					match[man_id] <- -1
				}
			}
			rival_id <- rival_id + 1
		}
		man_id <- man_id + 1
	}
	return(match)
}

# 付き合えるかどうかを判定
success_approach <- function(man, woman, match) {
	chance <- 0		# 付き合える可能性
	if (match[1] != -1) {
		chance = dexp(abs(0.5 - woman[match[1]]), rate = 1, log = FALSE)
		tmp_success <- tmp_success + chance
	}
	return(tmp_success)
}

for (p_num in num_person) {
	tmp_success <- 0	# 成功回数(一時的)

	for (g_num in goukon_num) {
		man <- c(my_charm)	# 男性陣
		woman <- c()		# 女性陣
		match <- c()		# アプローチ対象を記録

		man <- init_goukon_state_man(man, p_num)	# 合コンの場を仮想的に作る(男性陣)
		woman <- init_goukon_state_woman(woman, p_num)	# 合コンの場を仮想的に作る(女性陣)
		match <- to_approach(man, woman, match, my_lower_bound)	# アプローチする相手を選択
		match <- find_rival(man, woman, match)		# 一人の女性に複数のアプローチがあるときの処理
		tmp_success <- success_approach(man, woman, match)	# 付き合えるかどうかを判定
	}
success_num <- c(success_num,(tmp_success/length(goukon_num))*100);
}

# プロット
plot(success_num,
	type="b",
	xlab='合コンの組み合わせ (男性：女性)',
	ylab='私が付き合う可能性 (%)',
	xaxt='n'
)
xmonth<-c("1:1","2:2","3:3","4:4","5:5","6:6","7:7","8:8","9:9","10:10")
axis(side=1,at=1:10,labels=xmonth)

