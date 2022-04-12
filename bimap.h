#pragma once

#include <cstddef>
#include <functional>
#include <iostream>
#include <utility>

template <typename Left, typename Right, typename CompareLeft = std::less<Left>,
          typename CompareRight = std::less<Right>>
struct bimap {
  using left_t = Left;
  using right_t = Right;

  struct left_tag;
  struct right_tag;

  template <typename T, typename tag> struct node_side {
    typedef T key_t;
    node_side() : key(T{}){};

    node_side(node_side const &u) : right(u.right), left(u.left), key(u->key) {}

    explicit node_side(T &&elem) : key(std::move(elem)) {}

    explicit node_side(T const &elem) : key(elem) {}

    T &get() { return key; }

    node_side *right = nullptr;
    node_side *left = nullptr;
    T key;
  };

  using left_node_t = node_side<left_t, left_tag>;
  using right_node_t = node_side<right_t, right_tag>;

  struct node_t : left_node_t, right_node_t {
    node_t(left_t &&elem1, right_t &&elem2)
        : left_node_t(std::move(elem1)), right_node_t(std::move(elem2)) {}

    node_t(left_t &&elem1, right_t const &elem2)
        : left_node_t(std::move(elem1)), right_node_t(elem2) {}

    node_t(left_t const &elem1, right_t &&elem2)
        : left_node_t(elem1), right_node_t(std::move(elem2)) {}

    node_t(left_t const &elem1, right_t const &elem2)
        : left_node_t(elem1), right_node_t(elem2) {}
  };

  template <typename T, typename tag, typename Comp> struct tree : Comp {
    typedef T side_t;
    using tree_node_t = node_side<T, tag>;

    tree() : root(nullptr){};

    explicit tree(Comp comp) : Comp(comp), root(nullptr) {}

    void insert(tree_node_t *z) {
      root = insert(z, root);
      size++;
    }

    tree_node_t *search(T &&k) const { return search(root, std::move(k)); }

    tree_node_t *search(T const &k) const { return search(root, k); }

    void remove(tree_node_t *z, bool is_left_tree) {
      if (is_left_tree) {
        root = remove_nodes(root, z);
      } else {
        root = remove(root, z);
      }

      size--;
    }

    void destroy(bool is_left_tree) {
      root = destroy(root, is_left_tree);
      size = 0;
    }

    tree_node_t *minimum() const { return minimum(root); }

    tree_node_t *maximum() const { return maximum(root); }

    tree_node_t *next(tree_node_t *x) const {
      tree_node_t *cur = root;
      tree_node_t *succ = nullptr;
      while (cur != nullptr) {
        if (this->operator()(x->get(), cur->get())) {
          succ = cur;
          cur = cur->left;
        } else {
          cur = cur->right;
        }
      }
      return succ;
    }

    tree_node_t *prev(tree_node_t *x) const {
      tree_node_t *cur = root;
      tree_node_t *succ = nullptr;
      while (cur != nullptr) {
        if (this->operator()(x->get(), cur->get())) {

          cur = cur->left;
        } else {
          succ = cur;
          cur = cur->right;
        }
      }
      return succ;
    }

    tree_node_t *lower_bound(T const &x) const {
      tree_node_t *cur = root;
      tree_node_t *succ = nullptr;
      while (cur != nullptr) {
        if (this->operator()(x, cur->get())) {
          succ = cur;
          cur = cur->left;
        } else if (this->operator()(cur->get(), x)) {
          cur = cur->right;
        } else {
          succ = cur;
          break;
        }
      }
      return succ;
    }

    tree_node_t *upper_bound(T const &x) const {
      tree_node_t *cur = root;
      tree_node_t *succ = nullptr;
      while (cur != nullptr) {
        if (this->operator()(x, cur->get())) {
          succ = cur;
          cur = cur->left;
        } else {
          cur = cur->right;
        }
      }
      return succ;
    }

    bool is_empty() const { return root == nullptr; }

    std::size_t size_tree() const { return size; }

  private:
    tree_node_t *insert(tree_node_t *z, tree_node_t *x) {
      if (x == nullptr) {
        x = z;
      } else if (this->operator()(z->get(), x->get())) {
        x->left = insert(z, x->left);
      } else {
        x->right = insert(z, x->right);
      }
      return x;
    }

    tree_node_t *maximum(tree_node_t *x) const {
      if (x->right == nullptr) {
        return x;
      }
      return maximum(x->right);
    }

    tree_node_t *minimum(tree_node_t *x) const {
      if (x->left == nullptr) {
        return x;
      }
      return minimum(x->left);
    }

    tree_node_t *search(tree_node_t *x, T &&k) const {
      if (x == nullptr || k == x->get()) {
        return x;
      }
      if (this->operator()(k, x->get())) {
        return search(x->left, std::move(k));
      }
      return search(x->right, std::move(k));
    }

    tree_node_t *search(tree_node_t *x, T const &k) const {
      if (x == nullptr || k == x->get()) {
        return x;
      }
      if (this->operator()(k, x->get())) {
        return search(x->left, k);
      }
      return search(x->right, k);
    }

    tree_node_t *remove(tree_node_t *x, tree_node_t *z) {
      if (x == nullptr) {
        return x;
      }
      if (this->operator()(z->get(), x->get())) {
        x->left = remove(x->left, z);

      } else if (this->operator()(x->get(), z->get())) {
        x->right = remove(x->right, z);
      } else if (x->left != nullptr and x->right != nullptr) {
        auto old = x;
        x = minimum(x->right);
        x->right = remove(old->right, x);
        x->left = old->left;
      } else {
        if (x->left != nullptr) {
          x = x->left;
        } else if (x->right != nullptr) {
          x = x->right;
        } else {
          x = nullptr;
        }
      }
      return x;
    }

    tree_node_t *remove_nodes(tree_node_t *x, tree_node_t *z) {
      if (x == nullptr) {
        return x;
      }
      if (this->operator()(z->get(), x->get())) {
        x->left = remove_nodes(x->left, z);
      } else if (this->operator()(x->get(), z->get())) {
        x->right = remove_nodes(x->right, z);
      } else if (x->left != nullptr and x->right != nullptr) {
        auto old = x;
        x = minimum(x->right);
        x->right = remove(old->right, x);
        x->left = old->left;
        delete static_cast<node_t *>(old);
      } else {
        if (x->left != nullptr) {
          auto old = x;
          x = x->left;
          delete static_cast<node_t *>(old);
        } else if (x->right != nullptr) {
          auto old = x;
          x = x->right;
          delete static_cast<node_t *>(old);
        } else {
          delete static_cast<node_t *>(x);
          x = nullptr;
        }
      }
      return x;
    }

    tree_node_t *destroy(tree_node_t *x, bool is_left_tree) {
      if (x->right == nullptr && x->left == nullptr) {
        if (is_left_tree) {
          delete static_cast<node_t *>(x);
        }
        return nullptr;
      }
      if (x->right == nullptr) {
        x->left = destroy(x->left, is_left_tree);
      } else if (x->left == nullptr) {
        x->right = destroy(x->right, is_left_tree);
      } else {
        x->left = destroy(x->left, is_left_tree);
        x->right = destroy(x->right, is_left_tree);
      }
      if (is_left_tree) {
        delete static_cast<node_t *>(x);
      }
      return nullptr;
    }

    tree_node_t *root;
    std::size_t size = 0;
  };

  using left_tree_t = tree<left_t, left_tag, CompareLeft>;
  using right_tree_t = tree<right_t, right_tag, CompareRight>;

  template <typename side_node_t, typename an_side_node_t, typename side_tree,
            typename an_side_tree>
  struct iterator {
    typedef typename side_node_t::key_t side_key_t;
    typedef typename an_side_node_t::key_t an_side_key_t;

    iterator(node_t *node, side_tree const *this_tree,
             an_side_tree const *another_tree)
        : node(node), this_tree(this_tree), another_tree(another_tree){};

    side_key_t const &operator*() const {
      return static_cast<side_node_t *>(node)->get();
    };

    iterator &operator++() {
      node = static_cast<node_t *>(this_tree->next(node));
      return *this;
    };

    iterator operator++(int) {
      iterator old(node, this_tree, another_tree);
      node = static_cast<node_t *>(this_tree->next(node));
      return old;
    };
    iterator &operator--() {
      node = static_cast<node_t *>(this_tree->prev(node));
      return *this;
    };
    iterator operator--(int) {
      iterator old(node, this_tree, another_tree);
      node = static_cast<node_t *>(this_tree->prev(node));
      return old;
    };
    iterator<an_side_node_t, side_node_t, an_side_tree, side_tree>
    flip() const {
      return iterator<an_side_node_t, side_node_t, an_side_tree, side_tree>(
          this->node, this->another_tree, this->this_tree);
    };
    node_t *node;
    side_tree const *this_tree;
    an_side_tree const *another_tree;
  };

  using left_iterator =
      iterator<left_node_t, right_node_t, left_tree_t, right_tree_t>;
  using right_iterator =
      iterator<right_node_t, left_node_t, right_tree_t, left_tree_t>;

  friend bool operator==(left_iterator const &a, left_iterator const &b) {
    if (a.node == nullptr && b.node == nullptr)
      return true;
    return a.node == b.node;
  };

  friend bool operator!=(left_iterator const &a, left_iterator const &b) {
    return !(a == b);
  };

  friend bool operator==(right_iterator const &a, right_iterator const &b) {
    if (a.node == nullptr && b.node == nullptr)
      return true;
    return a.node == b.node;
  };

  friend bool operator!=(right_iterator const &a, right_iterator const &b) {
    return !(a == b);
  };

  // Создает bimap не содержащий ни одной пары.
  bimap(CompareLeft compare_left = CompareLeft(),
        CompareRight compare_right = CompareRight())
      : left_tree(compare_left), right_tree(compare_right){};

  // Конструкторы от других и присваивания
  bimap(bimap const &other)
      : left_tree(static_cast<CompareLeft>(other.left_tree)),
        right_tree(static_cast<CompareRight>(other.right_tree)) {

    for (auto it = other.begin_left(); it != other.end_left(); it++) {
      insert(*it, *it.flip());
    }
  };

  bimap(bimap &&other) noexcept { std::swap(*this, other); };

  void swap(bimap &a, bimap &b) noexcept {
    std::swap(a.left_tree, b.left_tree);
    std::swap(a.right_tree, b.right_tree);
  }

  bimap &operator=(bimap const &other) {
    bimap copy(other);
    swap(*this, copy);
    return *this;
  };

  bimap &operator=(bimap &&other) noexcept {
    swap(*this, other);
    return *this;
  };

  // Деструктор. Вызывается при удалении объектов bimap.
  // Инвалидирует все итераторы ссылающиеся на элементы этого bimap
  // (включая итераторы ссылающиеся на элементы следующие за последними).
  ~bimap() {
    if (!left_tree.is_empty()) {
      right_tree.destroy(false);
      left_tree.destroy(true);
    }
  };

  // Вставка пары (left, right), возвращает итератор на left.
  // Если такой left или такой right уже присутствуют в bimap, вставка не
  // производится и возвращается end_left().

  bool contains_any(left_t const &left, right_t const &right) {
    auto *node = static_cast<node_t *>(left_tree.search(left));
    if (node)
      return true;
    node = static_cast<node_t *>(right_tree.search(right));
    if (node)
      return true;
    return false;
  }

  //Не понимаю, как сократить копипасту в этом месте
  left_iterator insert(left_t const &left, right_t const &right) {
    if (contains_any(left, right)) {
      return end_left();
    }
    auto node = new node_t(left, right);
    return insert(node);
  };

  left_iterator insert(left_t const &left, right_t &&right) {
    if (contains_any(left, right)) {
      return end_left();
    }
    auto node = new node_t(left, std::move(right));
    return insert(node);
  };

  left_iterator insert(left_t &&left, right_t const &right) {
    if (contains_any(left, right)) {
      return end_left();
    }
    auto node = new node_t(std::move(left), right);
    return insert(node);
  };

  left_iterator insert(left_t &&left, right_t &&right) {
    if (contains_any(left, right)) {
      return end_left();
    }
    auto node = new node_t(std::move(left), std::forward<right_t>(right));
    return insert(node);
  };

  // Удаляет элемент и соответствующий ему парный.
  // erase невалидного итератора неопределен.
  // erase(end_left()) и erase(end_right()) неопределены.
  // Пусть it ссылается на некоторый элемент e.
  // erase инвалидирует все итераторы ссылающиеся на e и на элемент парный к e.
  left_iterator erase_left(left_iterator it) {
    auto old = left_iterator(static_cast<node_t *>(left_tree.next(it.node)),
                             &left_tree, &right_tree);
    remove(it.node);
    return old;
  };

  // Аналогично erase, но по ключу, удаляет элемент если он присутствует, иначе
  // не делает ничего Возвращает была ли пара удалена
  bool erase_left(left_t const &left) {
    auto *node = static_cast<node_t *>(left_tree.search(left));
    if (!node) {
      return false;
    }
    remove(node);
    return true;
  };

  right_iterator erase_right(right_iterator it) {
    auto old = right_iterator(static_cast<node_t *>(right_tree.next(it.node)),
                              &right_tree, &left_tree);
    remove(it.node);
    return old;
  };

  bool erase_right(right_t const &right) {
    auto *node = static_cast<node_t *>(right_tree.search(right));
    if (!node) {
      return false;
    }
    remove(node);
    return true;
  };

  // erase от ренжа, удаляет [first, last), возвращает итератор на последний
  // элемент за удаленной последовательностью
  left_iterator erase_left(left_iterator first, left_iterator last) {
    auto it = first;
    while (it != last) {
      auto old = it;
      it++;
      remove(old.node);
    }
    return left_iterator(static_cast<node_t *>(last.node), &left_tree,
                         &right_tree);
  };

  right_iterator erase_right(right_iterator first, right_iterator last) {
    auto it = first;
    while (it != last) {
      auto old = it;
      it++;
      remove(old.node);
    }
    return right_iterator(static_cast<node_t *>(last.node), &right_tree,
                          &left_tree);
  };

  // Возвращает итератор по элементу. Если не найден - соответствующий end()
  left_iterator find_left(left_t const &left) const {
    if (auto node = left_tree.search(left); node != nullptr) {
      return left_iterator(static_cast<node_t *>(node), &left_tree,
                           &right_tree);
    }
    return end_left();
  };

  right_iterator find_right(right_t const &right) const {
    if (auto node = right_tree.search(right); node != nullptr) {
      return right_iterator(static_cast<node_t *>(node), &right_tree,
                            &left_tree);
    }
    return end_right();
  };

  // Возвращает противоположный элемент по элементу
  // Если элемента не существует -- бросает std::out_of_range
  right_t const &at_left(left_t const &key) const {
    if (auto elem = static_cast<node_t *>(left_tree.search(key));
        elem != nullptr) {
      return static_cast<right_node_t *>(elem)->get();
    }
    throw std::out_of_range("element don't exist");
  };

  left_t const &at_right(right_t const &key) const {

    if (auto elem = static_cast<node_t *>(right_tree.search(key));
        elem != nullptr) {
      return static_cast<left_node_t *>(elem)->get();
    }
    throw std::out_of_range("element don't exist");
  };

  // Возвращает противоположный элемент по элементу
  // Если элемента не существует, добавляет его в bimap и на противоположную
  // сторону кладет дефолтный элемент, ссылку на который и возвращает
  // Если дефолтный элемент уже лежит в противоположной паре - должен поменять
  // соответствующий ему элемент на запрашиваемый (смотри тесты)
  right_t const &at_left_or_default(left_t const &key) {
    auto *elem = static_cast<node_t *>(left_tree.search(key));
    if (elem) {
      return static_cast<right_node_t *>(elem)->get();
    } else {
      if constexpr (std::is_default_constructible_v<right_t>) {
        elem = static_cast<node_t *>(right_tree.search(right_t()));
        if (elem) {
          erase_right(right_t());
        }
        left_iterator it = insert(key, right_t());
        return *(it.flip());
      }
      throw std::out_of_range("element don't exist");
    }
  };

  left_t const &at_right_or_default(right_t const &key) {
    auto *elem = static_cast<node_t *>(right_tree.search(key));
    if (elem) {
      return static_cast<left_node_t *>(elem)->get();
    } else {
      if constexpr (std::is_default_constructible<left_t>::value) {
        elem = static_cast<node_t *>(left_tree.search(left_t()));
        if (elem) {
          erase_left(left_t());
        }
        left_iterator it = insert(left_t(), key);
        return *it;
      }
      throw std::out_of_range("element don't exist");
    }
  };

  // lower и upper bound'ы по каждой стороне
  // Возвращают итераторы на соответствующие элементы
  // Смотри std::lower_bound, std::upper_bound.
  left_iterator lower_bound_left(const left_t &left) const {
    auto value = left_tree.lower_bound(left);
    if (value) {
      return left_iterator(static_cast<node_t *>(value), &left_tree,
                           &right_tree);
    }
    return end_left();
  };

  left_iterator upper_bound_left(const left_t &left) const {
    auto value = left_tree.upper_bound(left);
    if (value) {
      return left_iterator(static_cast<node_t *>(value), &left_tree,
                           &right_tree);
    }
    return end_left();
  };

  right_iterator lower_bound_right(const right_t &right) const {
    auto value = right_tree.lower_bound(right);
    if (value) {
      return right_iterator(static_cast<node_t *>(value), &right_tree,
                            &left_tree);
    }
    return end_right();
  };

  right_iterator upper_bound_right(const right_t &right) const {
    auto value = right_tree.upper_bound(right);
    if (value) {
      return right_iterator(static_cast<node_t *>(value), &right_tree,
                            &left_tree);
    }
    return end_right();
  };

  // Возващает итератор на минимальный по порядку left.
  left_iterator begin_left() const {
    if (size() > 0) {
      return left_iterator(static_cast<node_t *>(left_tree.minimum()),
                           &left_tree, &right_tree);
    }
    return left_iterator(static_cast<node_t *>(nullptr), &left_tree,
                         &right_tree);
  };

  // Возващает итератор на следующий за последним по порядку left.
  left_iterator end_left() const {
    return left_iterator(static_cast<node_t *>(nullptr), &left_tree,
                         &right_tree);
  };

  // Возващает итератор на минимальный по порядку right.
  right_iterator begin_right() const {
    if (size() > 0) {
      return right_iterator(static_cast<node_t *>(right_tree.minimum()),
                            &right_tree, &left_tree);
    }
    return right_iterator(static_cast<node_t *>(nullptr), &right_tree,
                          &left_tree);
  };

  // Возващает итератор на следующий за последним по порядку right.
  right_iterator end_right() const {
    return right_iterator(static_cast<node_t *>(nullptr), &right_tree,
                          &left_tree);
  };

  // Проверка на пустоту
  bool empty() const { return left_tree.is_empty(); };

  // Возвращает размер бимапы (кол-во пар)
  std::size_t size() const { return left_tree.size_tree(); };

  // операторы сравнения
  friend bool operator==(bimap const &a, bimap const &b) {
    if (a.size() != b.size()) {
      return false;
    }
    auto it1 = b.begin_left();
    for (auto it2 = a.begin_left(); it1 != b.end_left() && it2 != a.end_left();
         ++it2) {
      if (*it1 != *it2) {
        return false;
      }
      if (*(it1.flip()) != *(it2.flip())) {
        return false;
      }
      it1++;
    }

    return true;
  };

  friend bool operator!=(bimap const &a, bimap const &b) { return !(a == b); };

private:
  left_iterator insert(node_t *node) {
    left_tree.insert(static_cast<left_node_t *>(node));
    right_tree.insert(static_cast<right_node_t *>(node));
    return left_iterator(node, &left_tree, &right_tree);
  }

  void remove(node_t *node) {
    right_tree.remove(static_cast<right_node_t *>(node), false);
    left_tree.remove(static_cast<left_node_t *>(node), true);
  }
  left_tree_t left_tree;
  right_tree_t right_tree;
};
