#!/usr/bin/guile
!#

;;; Nemesis
;;; Copyright (C) 2017 Keith Bateman <kbateman@hawk.iit.edu>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(use-modules (ice-9 match)
			 (srfi srfi-1)
			 (srfi srfi-11)
			 (sly)
			 (sly fps)
			 ((sly math transform) #:prefix transform:)
			 (sly math quaternion)
			 (sly input keyboard)
			 (sly records)
			 (sly actor)
			 (sly audio)
			 (sly render)
			 (sly render camera)
			 (sly render color)
			 (sly render font)
			 (sly render framebuffer)
			 (sly render sprite)
			 (sly render sprite-batch)
			 (sly render tileset)
			 (sly utils)
			 (sly live-reload)
			 (sly signal))

(set! *random-state* (random-state-from-platform))

(enable-fonts)
(enable-audio)

(add-hook! key-press-hook (lambda (key)
                            (when (eq? key 'escape)
                              (stop-game-loop))))

(add-hook! window-close-hook stop-game-loop)

(define (display-fps fps)
  (format #t "FPS: ~d\n" fps))

;; (add-signal-hook! fps display-fps)

(define resolution (vector2 120 160))
(define resolution-scale 4)
(define scaled-resolution (v* resolution resolution-scale))
(define bounds (make-rect (vector2 0 0) scaled-resolution))
(define origin (vector2 240 320))

(define-record-type* <stats>
  %make-stats make-stats
  stats?
  (score stats-score 0)
  (lives stats-lives 3))

(define (decrement-life stats)
  (make-stats #:inherit stats
              #:lives (max 0 (1- (stats-lives stats)))))

(define (add-to-score enemy stats)
  ;; TODO: Award different points for different types of enemies.
  (make-stats #:inherit stats
              #:score (+ (stats-score stats)
                         1000)))

(define-record-type* <player>
  %make-player make-player
  player?
  (position player-position (vector2 0 -290))
  (direction player-direction (vector2 0 0))
  (shooting? player-shooting? #f)
  (hitbox player-hitbox (rect-inflate (make-rect 0 0 15 16) (vector2 4 4)))
  (last-death-time player-last-death-time #f))

(define %player-invincible-time (* 4 30))

(define (player-invincible? player time)
  (let ((last-death (player-last-death-time player)))
    (and last-death
         (negative?
          (- time (+ last-death %player-invincible-time))))))

(define (kill-player player time)
  ;; (display "gotcha")
  (make-player #:inherit player #:last-death-time time))

(define %player-bounds (make-rect (- (rect-half-width bounds)) (- (rect-half-height bounds)) (rect-width bounds) (- (rect-half-height bounds) 28)))

(define (player-world-hitbox player)
  (rect-move (player-hitbox player) (player-position player)))

(define (direct-player player direction)
  (make-player #:inherit player #:direction direction))

(define (move-player player offset)
  (make-player #:inherit player
			   #:position (rect-clamp %player-bounds
									  (v+ (player-position player)
										  offset))))

(define (player-forward speed)
  (lambda (world effects player)
	(values #f
			effects
			(move-player player (v* speed (player-direction player))))))

(define (set-player-shooting player shooting?)
  (make-player #:inherit player #:shooting? shooting?))

(define %bullet-bounds (make-rect (- (rect-half-width bounds)) (- (rect-half-height bounds)) (rect-width bounds) (rect-height bounds)))

(define-record-type* <bullet>
  %make-bullet make-bullet
  bullet?
  (live? bullet-live? #t)
  (position bullet-position origin)
  (direction bullet-direction 0)
  (hitbox bullet-hitbox (make-rect 0 0 3 3)))

(define (bullet-world-hitbox bullet)
  (rect-move (bullet-hitbox bullet) (bullet-position bullet)))

(define-record-type* <enemy>
  %make-enemy make-enemy
  enemy?
  (position enemy-position (vector2 0 290))
  (aim enemy-aim 0)
  (type enemy-type 'light)
  (health enemy-health 1)
  (hitbox enemy-hitbox (make-rect 0 0 15 16)))

(define (enemy-alive? enemy)
  (> (enemy-health enemy) 0))

(define (enemy-world-hitbox enemy)
  (rect-move (enemy-hitbox enemy) (enemy-position enemy)))

(define (damage-enemy enemy bullet time)
  (make-enemy #:inherit enemy
			  #:health (max 0
							(- (enemy-health enemy)
							   1))))

(define (make-light position action)
  (make-actor (make-enemy #:position position
						  #:health 1
						  #:type 'light
						  #:hitbox (rect-inflate (make-rect 0 0 10 12) (vector2 4 4)))
			  action))

(define (make-heavy position action)
  (make-actor (make-enemy #:position position
						  #:health 5
						  #:type 'heavy
						  #:hitbox (rect-inflate (make-rect 0 0 18 11) (vector2 4 4)))
			  action))

(define-record-type* <explosion>
  %make-explosion make-explosion
  explosion?
  (type explosion-type 'regular)
  (position explosion-position origin)
  (time explosion-time 0))

(define (explosion-active? explosion current-time)
  (< (- current-time (explosion-time explosion)) 15))

(define-record-type* <mirror>
  %make-mirror make-mirror
  mirror?
  (angle mirror-angle 0)
  (position mirror-pos 0)
  (vector mirror-vector (vector2 1 0)))

(define (mirror-player-pos mirror player)
  (v+ origin
	  (let ((m (slope (mirror-vector mirror)))
	  		(b (mirror-pos mirror)))
	  	(vector2 (/ (- (* 2 m b)) (+ 1 (* m m))) (/ (* 2 b) (+ 1 (* m m)))))
	  (transform:transform-vector2 (reflect (mirror-vector mirror) (mirror-pos mirror))
					 (actor-ref (call-with-actor player
												 player-position)))))
  
(define-record-type* <world>
  %make-world make-world
  world?
  (levels world-levels #f)
  (waves world-waves #f)
  (stats world-stats (make-stats))
  (player world-player (make-actor (make-player) idle))
  (enemies world-enemies '())
  (world-time world-timer 0)
  (player-bullets world-player-bullets '())
  (enemy-bullets world-enemy-bullets '())
  (mirrors world-mirrors '())
  (explosions world-explosions '()))

(define (game-over? world)
  (zero? (stats-lives (world-stats world))))

(define (mirror-bullets bullets world)
  (let accumulate-mirror-bullets ((mirs (world-mirrors world))
								  (mirrored-bullets '()))
	(cond ((null? mirs)
		   mirrored-bullets)
		  (else
		   (accumulate-mirror-bullets (cdr mirs)
									  (append (map (lambda (bullet)
													 (make-actor
													  (make-bullet #:inherit (actor-ref bullet)
																   #:position (v- (mirror-player-pos (car mirs)
																									 (world-player world))
																				  origin)
																   #:direction (- (+ (/ pi 2) (mirror-angle (car mirs)))))
													  player-bullet-script))
												   bullets)
											  mirrored-bullets))))))
  
(define (add-player-bullets world bullets)
  (make-world #:inherit world
			  #:player-bullets
			  (append bullets (world-player-bullets world))
			  #:enemy-bullets
			  (append (mirror-bullets bullets world)
					  (world-enemy-bullets world))))

(define (add-enemy-bullets world bullets)
  (make-world #:inherit world
			  #:enemy-bullets
			  (append bullets (world-enemy-bullets world))))

(define (move-bullet bullet offset)
  (make-bullet #:inherit bullet
			   #:position (v+ (bullet-position bullet) offset)))

(define (bullet-in-bounds? bullet)
  (rect-contains? %bullet-bounds (bullet-position bullet)))

(define (forward speed)
  (lambda (world effects bullet)
	(values #f
			effects
			(move-bullet bullet
						 (polar2 speed (bullet-direction bullet))))))

(define player-bullet-script
  (forever (forward 10)))

(define player-bullet-direction (/ pi 2))

(define (make-player-bullet player offset)
  (make-actor (make-bullet #:position (v+ (player-position player) offset)
						   #:direction player-bullet-direction)
			  player-bullet-script))

(define (player-shoot world player)
  (add-player-bullets world (list
							 (make-player-bullet player (vector2 0 0)))))

(define player-shoot* (action-effect-lift player-shoot))

(define (enemy-shoot world enemy speed aim-offset)
	  (let* ((position (enemy-position enemy))
			 (bullet (make-actor (make-bullet #:position position
											  #:direction (+ (enemy-aim enemy)
															 aim-offset))
								 (forever (forward speed)))))
		(add-enemy-bullets world (list bullet))))

(define enemy-shoot* (action-effect-lift enemy-shoot))

(define (move-enemy enemy offset)
  (make-enemy #:inherit enemy
			  #:position (v+ (enemy-position enemy) offset)))

(define move-enemy* (action-lift move-enemy))

;;;
;;; Collision detection
;;;

(define (keep-bullet? bullet)
  (and (bullet-live? bullet)
       (bullet-in-bounds? bullet)))

(define (enemy/player-collision? enemy player)
  (rect-intersects? (enemy-world-hitbox enemy)
                    (player-world-hitbox player)))

(define (enemy/bullet-collision? enemy bullet)
  (rect-intersects? (enemy-world-hitbox enemy)
                    (bullet-world-hitbox bullet)))


(define (player/bullet-collision? player bullet)
  (rect-intersects? (player-world-hitbox player)
                    (bullet-world-hitbox bullet)))

(define (collide-enemies-and-bullets enemies player-bullets stats time)
  (define (collide enemy bullets stats explosions)
    (let loop ((bullets bullets)
               (prev-bullets '())
               (stats stats))
      (match bullets
        (()
         (values enemy (reverse prev-bullets) stats explosions))
        ((bullet . rest)
         (if (enemy/bullet-collision? (actor-ref enemy) (actor-ref bullet))
             (let* ((new-enemy (call-with-actor enemy
                                (lambda (enemy)
                                  (damage-enemy enemy
                                                (actor-ref bullet)
                                                time))))
                    (new-enemy* (actor-ref new-enemy)))
               (values new-enemy
                       ;; Remove bullet.
                       (append (reverse prev-bullets) rest)
                       (if (enemy-alive? new-enemy*)
                           stats
                           ;; Enemy killed, add to player score
						   (add-to-score new-enemy* stats))
                       (if (enemy-alive? new-enemy*)
                           explosions
                           ;; Add new explosion.
                           (cons (make-explosion #:type 'regular
                                                 #:position (enemy-position
                                                             new-enemy*)
                                                 #:time time)
                                 explosions))))
             (loop rest (cons bullet prev-bullets) stats))))))
  
  (let loop ((enemies enemies)
             (new-enemies '())
             (bullets player-bullets)
             (explosions '())
             (stats stats))
    (match enemies
      (()
       (values (reverse new-enemies) bullets stats explosions))
      ((enemy . rest)
       (let-values (((new-enemy bullets stats explosions)
                     (collide enemy bullets stats explosions)))
         (loop rest
               (if (enemy-alive? (actor-ref new-enemy))
                   (cons new-enemy new-enemies)
                   new-enemies)
               bullets
               explosions
               stats))))))

(define (collide-player-and-enemies player enemies stats time)
  (let loop ((enemies enemies))
    (match enemies
      (()
       (values player stats '()))
      ((enemy . rest)
       (if (enemy/player-collision? (actor-ref enemy) (actor-ref player))
           (let* ((invincible? (player-invincible? (actor-ref player) time))
                  (new-player (if invincible?
                                  player
                                  (call-with-actor player
                                    (lambda (player)
                                      (kill-player player time)))))
                  (position (player-position (actor-ref player)))
                  (explosion (make-explosion #:type 'player
                                             #:position position
                                             #:time time)))
             (values new-player
                     (if invincible?
                         stats
                         (decrement-life stats))
                     (list explosion)))
           (loop rest))))))

(define (collide-player-and-bullets player enemy-bullets stats time)
  (let loop ((bullets enemy-bullets)
             (new-bullets '()))
    (match bullets
      (()
       (values player (reverse new-bullets) stats '()))
      ((bullet . rest)
       (let* ((b (actor-ref bullet))
              (p (actor-ref player)))
         (if (player/bullet-collision? p b)
             (let* ((hit? (not (player-invincible? p time)))
                    (new-player (if hit?
                                    (call-with-actor player
                                      (lambda (player)
                                        (kill-player player time)))
                                    player))
                    (position (player-position p))
                    (explosion (make-explosion #:type 'player
                                               #:position position
                                               #:time time)))
               (values new-player
                       (append (reverse new-bullets) rest)
                       (if hit? (decrement-life stats) stats)
                       (if hit? (list explosion) '())))
             (loop rest (cons bullet new-bullets))))))))

(define (wave-1-1)
  (list (make-light (vector2 (rect-half-width bounds) 200)
					(forever
					 (sequence
					   (repeat 240
							   (move-enemy* (vector2 -2 0)))
					   (repeat 240
							   (move-enemy* (vector2 2 0))))))))
					 
(define (wave-1-2)
  (list (make-light (vector2 (rect-half-width bounds) 200)
					(forever
					 (sequence
					   (repeat 10
							   (sequence
								 (both
								  (move-enemy* (vector2 -2 0))
								  (enemy-shoot* 5 (- (/ pi 2))))
								 (repeat 23
										 (move-enemy* (vector2 -2 0)))))
					   (repeat 10
							   (sequence
								 (both
								  (move-enemy* (vector2 2 0))
								  (enemy-shoot* 5 (- (/ pi 2))))
								 (repeat 23
										 (move-enemy* (vector2 2 0))))))))))

(define (wave-1-3)
  (list (make-light (vector2 (rect-half-width bounds) 200)
					(forever
					 (sequence
					   (repeat 3
							   (sequence
								 (both
								  (enemy-shoot* 5 (- (/ pi 2)))
								  (move-enemy* (vector2 -5 0)))
								 (repeat 13
										 (move-enemy* (vector2 -5 0)))))
					   (repeat 3
							   (sequence
								 (both
								  (enemy-shoot* 5 (- (/ pi 2)))
								  (move-enemy* (vector2 5 0)))
								 (repeat 13
										 (move-enemy* (vector2 5 0))))))))
		(make-light (vector2 (- (rect-half-width bounds)) 200)
					(forever
					 (sequence
					   (repeat 3
							   (sequence
								 (both
								  (enemy-shoot* 5 (- (/ pi 2)))
								  (move-enemy* (vector2 5 0)))
								 (repeat 13
										 (move-enemy* (vector2 5 0)))))
					   (repeat 3
							   (sequence
								 (both
								  (enemy-shoot* 5 (- (/ pi 2)))
								  (move-enemy* (vector2 -5 0)))
								 (repeat 13
										 (move-enemy* (vector2 -5 0))))))))))

(define (wave-1-4)
  (append (wave-1-2)
		  (list (make-heavy (vector2 0 200)
							(forever
							 (sequence
							   (enemy-shoot* 5 (- (/ pi 2)))
							   (wait 9)
							   (enemy-shoot* 5 (- (* .4 pi)))
							   (wait 9)
							   (enemy-shoot* 5 (- (* .6 pi)))
							   (wait 9)))))))

(define %mirrors
  (list
   (make-mirror)
   ;; (make-mirror #:angle (- (/ pi 4))
   ;; 				#:vector (vector2 1 1))
   ;; (make-mirror #:angle (/ pi 4)
   ;; 				#:vector (vector2 -1 1))
   ))

(define %waves
  (list (wave-1-1)
		(wave-1-2)
		(wave-1-3)
		(wave-1-4)))

(define %default-player
  (make-actor (make-player)
              (forever
			   (sequence
				 (both
				  (player-forward 8)
				  (whena player-shooting?
						 (player-shoot*)))
				 (repeat 11 (player-forward 8))))))

(define %default-world
  (make-world #:player %default-player
			  #:waves %waves
			  #:mirrors %mirrors))

(define (update-bullets effects world bullets)
  (values effects
		  (filter-map (lambda (actor)
						(let-values (((effects new-actor)
									  (update-actor world '() actor)))
						  (let ((bullet (actor-ref new-actor)))
							(and (bullet-live? bullet)
								 (bullet-in-bounds? bullet)
								 new-actor))))
					  bullets)))

(define (update-player effects world)
  (update-actor world effects (world-player world)))

(define (update-enemies effects world)
  (let-values (((new-effects new-enemies)
				(actor-filter-update enemy-alive? world (world-enemies world))))
	(values (append new-effects effects) new-enemies)))

(define (update-world world time)
  (let*-values (((game-over?) (game-over? world))
				((effects new-player)
				 (update-player '() world))
				((effects new-enemies)
				 (update-enemies effects world))
			   ((effects new-player-bullets)
				(update-bullets effects world (world-player-bullets world)))
			   ((effects new-enemy-bullets)
				(update-bullets effects world (world-enemy-bullets world)))
			   ((stats) (world-stats world))
			   ((new-enemies new-player-bullets new-stats explosions1)
				;; TODO: Don't allow enemies to be killed after the game has been
				;; lost because that would lead to strange things.
				(collide-enemies-and-bullets new-enemies new-player-bullets
											 stats time))
			   ((new-enemies new-waves)
				(let ((waves (world-waves world)))
				  (cond ((not waves)
						 (values new-enemies #f))
						((null? waves)
						 (values new-enemies '()))
						((null? new-enemies)
						 (values (car waves) (cdr waves)))
						(else
						 (values new-enemies waves)))))
			   ((new-player new-enemy-bullets new-stats explosions2)
				;; TODO: Don't collide when the game has been won or lost.
				(collide-player-and-bullets new-player new-enemy-bullets
											new-stats time))
			   ((new-explosions)
				(filter (lambda (explosion)
						  (explosion-active? explosion time))
						(append explosions1
								explosions2
								(world-explosions world)))))
	(apply-effects effects
				   (make-world #:player new-player
							   #:player-bullets new-player-bullets
							   #:enemies new-enemies
							   #:enemy-bullets new-enemy-bullets
							   #:mirrors (world-mirrors world)
							   #:stats new-stats
							   #:waves new-waves
							   #:explosions new-explosions
							   #:world-time time))))

(define (world-eval exp world)
  (match exp
	(('null) world)
	(('tick time)
	 (update-world world time))
	(('player-direction direction)
	 (make-world #:inherit world
				 #:player (call-with-actor (world-player world)
										   (lambda (player)
											 (direct-player player direction)))))
	(('player-shoot shooting?)
	 (make-world #:inherit world
				 #:player (call-with-actor (world-player world)
										   (lambda (player)
											 (set-player-shooting player shooting?)))))))

(define-signal world
  (signal-fold world-eval
			   %default-world
			   (signal-merge
				(make-signal '(null))
				(signal-let ((time (signal-timer)))
							`(tick ,time))
				(signal-let ((direction key-arrows))
							`(player-direction ,direction))
				(signal-let ((shoot? (signal-drop-repeats (key-down? 'z))))
							`(player-shoot ,shoot?)))))

(define-signal background-sprite
  ((with-live-reload load-sprite) "bg.png"))

(define-signal mirror-sprite
  ((with-live-reload load-sprite) "mirror.png"))

(define-signal bullet-sprite
  ((with-live-reload load-sprite) "bullet.png"))

(define-signal enemy-bullet-sprite
  ((with-live-reload load-sprite) "enemy-bullet.png"))

(define-signal player-tileset
  ((with-live-reload load-tileset) "helix-piston-player.png" 15 16))

(define-signal enemy-heavy-tileset
  ((with-live-reload load-tileset) "enemy-heavy.png" 18 11))

(define-signal enemy-light-tileset
  ((with-live-reload load-tileset) "enemy-light.png" 10 12))

(define-signal enemy-explosion-tileset
  ((with-live-reload load-tileset) "enemy-explosion.png" 13 16))

(define-signal mirror-player-tileset
  ((with-live-reload load-tileset) "mirror-player.png" 15 16))

(define-signal player-sprite
  (signal-let ((time (signal-sample 10 (signal-timer)))
			   (player-tileset player-tileset))
			  (if player-tileset
				  (make-sprite
				   (tileset-ref player-tileset (- 2 (remainder time 3))))
				  null-sprite)))

(define-signal mirror-player-sprite
  (signal-let ((time (signal-sample 10 (signal-timer)))
			   (mirror-player-tileset mirror-player-tileset))
			  (if mirror-player-tileset
				  (make-sprite
				   (tileset-ref mirror-player-tileset (+ 0 (remainder time 3))))
				  null-sprite)))

(define-signal enemy-heavy-sprite
  (signal-let ((time (signal-sample 5 (signal-timer)))
			   (enemy-heavy-tileset enemy-heavy-tileset))
			  (if enemy-heavy-tileset
				  (make-sprite
				   (tileset-ref enemy-heavy-tileset (- 2 (remainder time 3))))
				  null-sprite)))

(define-signal enemy-light-sprite
  (signal-let ((time (signal-sample 4 (signal-timer)))
			   (enemy-light-tileset enemy-light-tileset))
			  (if enemy-light-tileset
				  (make-sprite
				   (tileset-ref enemy-light-tileset (- 2 (remainder time 3))))
				  null-sprite)))

(define-signal enemy-explosion-sprite
  (signal-let ((time (signal-sample 5 (signal-timer)))
			   (enemy-explosion-tileset enemy-explosion-tileset))
			  (if enemy-explosion-tileset
				  (make-sprite
				   (tileset-ref enemy-explosion-tileset (+ 0 (remainder time 3))))
				  null-sprite)))

(define (reflect-y)
  (reflect (vector2 1 0) 0))

(define (slope v)
  (if (vector2? v)
	  (if (= (vx v) 0)
		  'undefined
		  (/ (vy v) (vx v)))
	  (error "slope -- NOT VECTOR2" v)))
	  

(define (reflect vector b)
  (let ((m (slope vector)))
	(cond ((eq? m 'undefined)
		   (transform:make-transform -1 0 0 0
									 0 1 0 0
									 0 0 1 0
									 0 0 0 1))
		  (else
		   (transform:make-transform (/ (- 1 (* m m)) (+ 1 (* m m))) (/ (* 2 m) (+ 1 (* m m))) 0 0
									 (/ (* 2 m) (+ 1 (* m m))) (/ (- (* m m) 1) (+ 1 (* m m))) 0 0
									 0 0 1 0
									 0 0 0 1)))))

;; (define-signal mirror-player-pos
;;   (signal-map (lambda (world)
;; 				(transform-vector2 (reflect-y) (actor-ref (call-with-actor (world-player world)
;; 																		   player-position))))
;; 			  world))

;; (define (display-bullets player-bullets)
;;   (format #t "Bullets: ~A\n" player-bullets))

;; (define (make-player-bullet player-pos)
;;   (signal-fold v+ player-pos
;; 			   (signal-map (lambda (x) (vector2 0 (+ (/ x 2)))) (signal-timer))))

;; (define-signal player-bullet
;;   (signal-if (signal-sample 10 (key-down? 'z))
;; 			 (make-player-bullet (actor-ref (call-with-actor (world-player (signal-ref world))
;; 															 player-position)))
;; 			 (make-signal #f)))

;; (define-signal player-bullets
;;   (signal-fold cons '()
;; 			   (signal-drop-repeats (signal-map identity player-bullet))))
  
(define camera
  (2d-camera #:area (make-rect 0 0 480 640)))

(define-signal scene
  (signal-let ((background-sprite background-sprite)
			   (mirror-sprite mirror-sprite)
			   (bullet-sprite bullet-sprite)
			   (enemy-bullet-sprite enemy-bullet-sprite)
			   (player-sprite player-sprite)
			   (enemy-light-sprite enemy-light-sprite)
			   (enemy-heavy-sprite enemy-heavy-sprite)
			   (enemy-explosion-sprite enemy-explosion-sprite)
			   (mirror-player-sprite mirror-player-sprite)
			   (world world))
			  (with-camera camera
						   (scale resolution-scale
								  (render-begin
								   (if background-sprite
								   	   (move origin (render-sprite background-sprite))
								   	   render-nothing)
								   ;; Accumulate mirror renderers
								   (if (and mirror-sprite
											mirror-player-sprite)
									   (let render-mirrors ((mirs (world-mirrors world))
															(mirror-render-list (list render-nothing)))
											  (cond ((null? mirs)
													 (list->renderer mirror-render-list))
													(else
													 (render-mirrors
													  (cdr mirs)
													  (cons*
													   (rotate-z
														(mirror-angle (car mirs))
														(move (v+ origin (vector2 0 (mirror-pos (car mirs))))
															  (render-sprite mirror-sprite)))
													   (rotate-z
														(mirror-angle (car mirs))
														(move (mirror-player-pos (car mirs) (world-player world))
															  (render-sprite mirror-player-sprite)))
													   mirror-render-list)))))
											render-nothing)
								   (move origin
										 (render-begin
										  (if (not (null? (world-player-bullets world)))
											  (list->renderer
											   (map (lambda (x) (if x
																	(move (actor-ref (call-with-actor x
																									  bullet-position))
																		  (render-sprite bullet-sprite))
																	render-nothing))
													(world-player-bullets world)))
											  render-nothing)
										  (if (not (null? (world-enemy-bullets world)))
											  (list->renderer
											   (map (lambda (x) (if (and x enemy-bullet-sprite)
																	(move (actor-ref (call-with-actor x
																									  bullet-position))
																		  (render-sprite enemy-bullet-sprite))
																	render-nothing))
													(world-enemy-bullets world)))
											  render-nothing)
										  (if player-sprite
											  (move (actor-ref (call-with-actor (world-player world)
																				player-position))
													(render-sprite player-sprite))
											  render-nothing)
										  (list->renderer
										   (map (lambda (x)
												  (let ((type (actor-ref (call-with-actor x enemy-type)))
														(position (actor-ref (call-with-actor x enemy-position))))
													(cond ((eq? type 'light)
														   (if enemy-light-sprite
															   (move position
																	 (render-sprite enemy-light-sprite))
															   render-nothing))
														  ((eq? type 'heavy)
														   (if enemy-heavy-sprite
															   (move position
																	 (render-sprite enemy-heavy-sprite))
															   render-nothing))
														  (else
														   render-nothing))))
												(world-enemies world)))
										  (if (not (null? (world-explosions world)))
											  (list->renderer
											   (map (lambda (x)
													  (if (and x enemy-explosion-sprite)
														  (move (explosion-position x)
																(render-sprite enemy-explosion-sprite))
														  render-nothing))
													(world-explosions world)))
											  render-nothing))))))))

(with-window (make-window #:title "Nemesis" #:resolution scaled-resolution)
			 (start-sly-repl)
			 (let ((music (load-music "Mercury.wav")))
			   (play-music music #:loop? #t))
			 (run-game-loop scene))
