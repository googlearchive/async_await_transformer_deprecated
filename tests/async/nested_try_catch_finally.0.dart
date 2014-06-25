main_async() {
  await(f());
}

S_async(n, throws) {
  print(n);
  if (throws) {
    throw n;
  } else {
  }
}

f_async() {
  try {
    try {
      try {
        try {
          await(S(1, 1));
        } catch (e) {
          await(S(2, 1));
        }
      } finally {
        await(S(3, 1));
      }
    } catch (e) {
      try {
        try {
          await(S(4, 1));
        } catch (e) {
          await(S(5, 1));
        }
      } finally {
        await(S(6, 1));
      }
    }
  } finally {
    try {
      try {
        await(S(6, 1));
      } catch (e) {
        await(S(7, 1));
      }
    } finally {
      await(S(8, 0));
    }
  }
}

